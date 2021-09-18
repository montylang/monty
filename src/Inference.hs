{-# LANGUAGE DataKinds #-}
module Inference where

import MyPrelude

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import PrettyPrint
import Control.Monad.State
import ParserTypes (Id, Arg (IdArg, _idArgVal), CondBlock (CondBlock))
import Control.Monad.Except
import MorphUtils
import MiddleEndTypes
import Debug.Trace (trace)
import Data.List

type TIState = Int
type TI a = ExceptT String (State TIState) a

class Types a where
  -- Find all FreeTypeVariables in something
  ftv :: a -> HS.HashSet String
  -- Applies a substitution (of types) to something
  substitute :: Subst -> a -> a

instance Types a => Types [a] where
    substitute s = map (substitute s)
    ftv l        = foldr (HS.union . ftv) HS.empty l

newtype TypeEnv = TypeEnv (HM.HashMap String Scheme)
  deriving (Show)
emptyTypeEnv = TypeEnv HM.empty

instance Types TypeEnv where
  ftv (TypeEnv env)          = ftv (HM.elems env)
  substitute s (TypeEnv env) = TypeEnv (HM.map (substitute s) env)

type Subst = HM.HashMap String MType

nullSubst :: Subst
nullSubst = HM.empty

runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) 0

data Scheme = Scheme
  { schemaTypeVars :: [String]
  , schemaType :: MType }
  deriving (Show)

instance Types Scheme where
    ftv (Scheme vars t) = HS.difference (ftv t) (HS.fromList vars)
    substitute s (Scheme vars t) = Scheme vars (substitute (foldr HM.delete s vars) t)

instance Types MType where
    ftv MInt        = HS.empty
    ftv MBool       = HS.empty
    ftv (MVar name) = HS.singleton name
    ftv (MFun inputType outputType) =
      HS.unions $ ftv <$> [outputType, inputType]

    substitute s (MVar name) = case HM.lookup name s of
                         Nothing  -> MVar name
                         Just t   -> t
    substitute s (MFun t1 t2) = MFun (substitute s t1) (substitute s t2)
    substitute s t            = t

-- Creating a new polymorphic type var, a, b, ... a1, b1, etc.
newTyVar :: TI MType
newTyVar = do
  s <- get
  put $ s + 1
  pure $ MVar $ reverse $ toTyVar s

-- a, b, ..., a1, b1, ...
toTyVar :: Int -> String
toTyVar c | c < 26    = [toEnum (97+c)]
          | otherwise = let (name, r) = c `divMod` 26
                        in toEnum (97+r) : toTyVar (name-1)

instantiate :: Scheme -> TI MType
instantiate (Scheme vars t) = do
  -- Create polymorphic type var list to match `vars`
  nvars <- mapM (const newTyVar) vars
  -- Map source code names -> type var names
  let s = HM.fromList $ zip vars nvars
  -- Apply substitution to the vars, if appropriate
  pure $ substitute s t

removeProgramVar :: TypeEnv -> Arg -> TypeEnv
removeProgramVar (TypeEnv env) arg = TypeEnv (HM.delete (unpackIdArg arg) env)

unpackIdArg :: Arg -> Id
unpackIdArg (IdArg name) = name
unpackIdArg _ = trace "pattern match args not supported yet" undefined

-- TODO: Figure out what the hell this is for
-- What we _think_ it's for:
-- Comes up with a scheme that "links" type variables in the given type to the env
-- aka, ban alpha-substitution in the schema
generalize :: TypeEnv -> MType -> Scheme
generalize env@(TypeEnv envContent) t = Scheme vars t
  -- Free type vars in the type but not free in the env
  where vars = HS.toList (HS.difference (ftv t) (ftv env))
  -- difference (fromList [5, 3]) (fromList [5, 7]) == singleton 3

-- Simplifies the type vars of a scheme
-- Should only be called at the root scope!
simplifyType :: MType -> MType
simplifyType t = substitute sub t
  where
  vars    = sort $ HS.toList $ ftv t
  newVars = toTyVar <$> [0..length vars - 1]
  sub     = HM.fromList $ zip vars (MVar <$> newVars)

-- This is the unification function for types
-- Example: unify (a -> a) (int -> b) = Map(a = int, b = int)
unify :: MType -> MType -> TI Subst
unify (MFun l r) (MFun l' r') = do
  s1 <- unify l l'
  s2 <- unify (substitute s1 r) (substitute s1 r')
  return $ composeSubst s1 s2
unify (MVar u) t  = varBind u t
unify t (MVar u)  = varBind u t
unify MInt MInt   = return nullSubst
unify MBool MBool = return nullSubst
unify t1 t2       = throwError $ "types do not unify: " ++ show t1 ++
                  " vs. " ++ show t2

-- The function varBind attempts to bind a type variable
-- to a type and return that binding as a subsitution,
-- but avoids binding a variable to itself and
-- performs the occurs check.
varBind :: String -> MType -> TI Subst
varBind u t | t == MVar u          = return nullSubst
            -- Case where \x -> x x
            | HS.member u (ftv t) = throwError $ "occurs check fails: " ++ u ++
                                      " vs. " ++ show t
            | otherwise            = return $ HM.singleton u t

-- TODO: Figure out why s1 has precedence
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = HM.union (HM.map (substitute s1) s2) s1

-- We treat assignments in functions like "let" expressions
tiLet :: TypeEnv -> Arg -> ExistsMExpr -> [ExistsMExpr] -> TI (Subst, MType, TypeEnv)
tiLet env lhs rhs body = do
  let IdArg lhsName = lhs
  (rhsSubst, rhsType, newEnv) <- ti env rhs
  let TypeEnv env' = removeProgramVar newEnv lhs
      t'           = generalize (substitute rhsSubst env) rhsType
      env''        = TypeEnv (HM.insert lhsName t' env')
  (s2, t2, _) <- tiBody (substitute rhsSubst env'') body
  pure (composeSubst rhsSubst s2, t2, newEnv)

tiBody :: TypeEnv -> [ExistsMExpr] -> TI (Subst, MType, TypeEnv)
tiBody env []  = throwError "Empty body"
tiBody env [x] = ti env x
tiBody env ((Exists (MExprAssignment _ lhs rhs):xs)) =
  tiLet env lhs rhs xs
tiBody env (_:xs) = tiBody env xs

tiCondBlock :: TypeEnv -> CondBlock ExistsMExpr -> TI (Subst, MType, TypeEnv)
tiCondBlock env (CondBlock cond body) = do
  condSub <- tiCond cond
  over _1 (composeSubst condSub) <$> tiBody env body
  where
    tiCond :: ExistsMExpr -> TI Subst
    tiCond cond = do
      (condSub, condType, _) <- ti env cond
      condSub' <- unify (substitute condSub condType) MBool

      let condType' = substitute condSub' condType
      unless (condType' == MBool) $ throwError "Condition must be a boolean"

      pure $ composeSubst condSub' condSub

-- Type inferrence
ti :: TypeEnv -> ExistsMExpr -> TI (Subst, MType, TypeEnv)
-- Hack until we support data
ti env (Exists (MExprId _ "True")) = pure (nullSubst, MBool, env)
ti env (Exists (MExprId _ "False")) = pure (nullSubst, MBool, env)
ti env (Exists MExprInt {}) = pure (nullSubst, MInt, env)
ti env (Exists (MExprAssignment _ lhs rhs)) = do
  let TypeEnv envMap = env
  let lhsName = unpackIdArg lhs

  when (HM.member lhsName envMap)
    (throwError $ lhsName <> " is already defined")

  -- Define the assignment to be a new type var, to fix recursive inference
  recScheme <- generalize env <$> newTyVar
  let recEnv = TypeEnv (HM.insert lhsName recScheme envMap)

  (rhsSubst, rhsType, _) <- ti recEnv rhs

  let scheme = generalize (substitute rhsSubst recEnv) rhsType
  let newEnv = TypeEnv (HM.insert lhsName scheme envMap)

  pure (rhsSubst, rhsType, newEnv)
ti env (Exists (MExprId _ name)) =
  let TypeEnv envMap = env in
  case HM.lookup name envMap of
    Nothing    -> throwError $ "unbound variable: " ++ name
    Just sigma -> do
      t <- instantiate sigma
      pure (nullSubst, t, env)
ti env (Exists (MExprDef _ args body)) = do
  tvs <- sequence $ newTyVar <$ args
  let idArgs = unpackIdArg <$> args

  -- Remove the function params from the env, aka "shadowing"
  let TypeEnv env' = foldl removeProgramVar env args
  -- New env, with the newly created var in env''
  let env'' = TypeEnv $ HM.union env' $ HM.fromList $ zip idArgs (Scheme [] <$> tvs)

  (subBody, tbody, _) <- tiBody env'' body

  let tres = case tvs of
        []  -> MFunZero tbody
        tvs -> foldr MFun tbody (substitute subBody <$> tvs)

  pure (subBody, tres, env)
ti env (Exists (MExprCall _ fexpr params)) = do
  (fsub, ftype, _) <- ti env fexpr
  let env' = substitute fsub env

  (resSub, resType) <- foldM (linkParamTypes env') (fsub, ftype) params
  pure (resSub, resType, env)
  where
    linkParamTypes :: TypeEnv -> (Subst, MType) -> ExistsMExpr -> TI (Subst, MType)
    linkParamTypes env (prevSub, prevType) paramExpr = do
      (paramSub, paramType, _) <- ti env paramExpr
      tv     <- newTyVar
      retSub <- unify (substitute paramSub prevType) (MFun paramType tv)
      pure (retSub `composeSubst` paramSub `composeSubst` prevSub, substitute retSub tv)
ti env (Exists (MExprIfElse _ ifCond [] elseBody)) = do
  (ifSub, ifType, env')      <- tiCondBlock env ifCond
  (elseSub, elseType, env'') <- tiBody env' elseBody

  retSub <- unify (substitute ifSub elseType) ifType
  
  pure (retSub `composeSubst` ifSub `composeSubst` elseSub, ifType, env'')
ti _ expr = trace (show expr) undefined

findAssignments :: [ExistsMExpr] -> HM.HashMap String ExistsMExpr
findAssignments exprs = HM.fromList $ foldl' ass [] exprs
  where
    ass :: [(String, ExistsMExpr)] -> ExistsMExpr -> [(String, ExistsMExpr)]
    ass acc expr@(Exists MExprAssignment {_lhs}) =
      (unpackIdArg _lhs, expr) : acc
    ass acc _ = acc

-- Should only be called at the root scope
inferMExprs :: TypeEnv -> [ExistsMExpr] -> TI [MType]
inferMExprs env exprs =
  (simplifyType <$>) . reverse . view _2 <$> foldM feed (env, []) exprs
  where
    feed :: (TypeEnv, [MType]) -> ExistsMExpr -> TI (TypeEnv, [MType])
    feed (env, prev) expr = do
      (s, t, env') <- ti env expr
      pure (env', substitute s t : prev)
