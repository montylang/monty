{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Inference where

import MyPrelude

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import PrettyPrint
import Control.Monad.State
import ParserTypes (Id, Arg (IdArg, _idArgVal), CondBlock (CondBlock))
import Control.Monad.Except
import MiddleEndTypes
import Debug.Trace (trace)
import Data.List
import InferenceTypes
import Control.Applicative
import Text.Megaparsec (parse, SourcePos (SourcePos))
import Parser.TypeParser (parseSig)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Pos (mkPos)

parseTypeOrDie :: String -> MType
parseTypeOrDie s = case parse parseSig "" s of
  Left err -> trace (errorBundlePretty err) undefined
  Right res -> res

builtinTypes :: HM.HashMap String MType
builtinTypes = HM.fromList [
    ("#add",      parseTypeOrDie "Int -> Int -> Int"),
    ("#subtract", parseTypeOrDie "Int -> Int -> Int"),
    ("#multiply", parseTypeOrDie "Int -> Int -> Int"),
    ("#or",       parseTypeOrDie "Bool -> Bool -> Bool"),
    ("#not",      parseTypeOrDie "Bool -> Bool"),
    ("#equals",   parseTypeOrDie "a -> a -> Bool"),
    ("const",     parseTypeOrDie "a -> b -> a"),
    -- fix cannot inherently be implicitly typed unfortunately
    ("fix",       parseTypeOrDie "(a -> a) -> a")
  ]

-- Creating a new polymorphic type var, a, b, ... a1, b1, etc.
newTyVar :: TI MType
newTyVar = do
  s <- use tyVarCount
  tyVarCount .= s + 1
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
unify t1 t2       = throwError [i|"types do not unify: ${t1} vs ${t2}"|]

-- The function varBind attempts to bind a type variable
-- to a type and return that binding as a subsitution,
-- but avoids binding a variable to itself and
-- performs the occurs check.
varBind :: String -> MType -> TI Subst
varBind u t | t == MVar u          = return nullSubst
            -- Case where \x -> x x
            | HS.member u (ftv t) = throwError [i|"occurs check fails: ${u} vs ${t}"|]
            | otherwise           = return $ HM.singleton u t

-- TODO: Figure out why s1 has precedence
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = HM.union (HM.map (substitute s1) s2) s1

-- We treat assignments in functions like "let" expressions
tiLet :: TypeEnv -> Arg -> ExistsMExpr -> [ExistsMExpr] -> TI (Subst, MType)
tiLet env lhs rhs body = do
  let IdArg lhsName = lhs
  (rhsSubst, rhsType) <- ti env rhs
  let TypeEnv env' = removeProgramVar env lhs
      t'           = generalize (substitute rhsSubst env) rhsType
      env''        = TypeEnv (HM.insert lhsName t' env')
  (s2, t2) <- tiBody (substitute rhsSubst env'') body
  pure (composeSubst rhsSubst s2, t2)

tiBody :: TypeEnv -> [ExistsMExpr] -> TI (Subst, MType)
tiBody env []  = throwError "Empty body"
tiBody env [x] = ti env x
tiBody env ((Exists (MExprAssignment _ lhs rhs):xs)) =
  tiLet env lhs rhs xs
tiBody env (x:xs) = tiBody env xs

tiCondBlock :: TypeEnv -> CondBlock ExistsMExpr -> TI (Subst, MType)
tiCondBlock env (CondBlock cond body) = do
  condSub <- tiCond cond
  over _1 (composeSubst condSub) <$> tiBody env body
  where
    tiCond :: ExistsMExpr -> TI Subst
    tiCond cond = do
      (condSub, condType) <- ti env cond
      condSub' <- unify (substitute condSub condType) MBool

      let condType' = substitute condSub' condType
      unless (condType' == MBool) $ throwError "Condition must be a boolean"

      pure $ composeSubst condSub' condSub

-- Type inferrence
ti :: TypeEnv -> ExistsMExpr -> TI (Subst, MType)
-- Hack until we support data
ti env (Exists (MExprId _ "True")) = pure (nullSubst, MBool)
ti env (Exists (MExprId _ "False")) = pure (nullSubst, MBool)
ti env (Exists MExprInt {}) = pure (nullSubst, MInt)
ti env (Exists (MExprAssignment _ lhs rhs)) = do
  let TypeEnv envMap = env
  let lhsName = unpackIdArg lhs

  when (HM.member lhsName envMap)
    (throwError $ lhsName <> " is already defined")

  (rhsSubst, rhsType) <- ti env rhs

  let scheme = generalize (substitute rhsSubst env) rhsType
  let newEnv = TypeEnv (HM.insert lhsName scheme envMap)

  pure (rhsSubst, rhsType)
ti env (Exists (MExprId _ name)) = do
  t <- localLookup <|> globalLookup <|> globalInferLookup
  pure (nullSubst, t)
  where
    -- TODO: Don't throw away the result
    -- A simple solution would be to just modify the global defs list right here
    globalInferLookup :: TI MType
    globalInferLookup = do
      glob <- use topLevelMExprs

      case HM.lookup name glob of
        Nothing -> throwError $ "Unbound variable: " ++ name
        Just ex -> do
          view _2 <$> ti env ex

    localLookup :: TI MType
    localLookup = do
      let TypeEnv envMap = env
    
      case HM.lookup name envMap of
        Nothing    -> throwError ""
        Just sigma -> instantiate sigma

    globalLookup :: TI MType
    globalLookup = do
      glob <- use globalDefs

      case HM.lookup name glob of
        Nothing    -> throwError ""
        Just sigma -> instantiate $ generalize env sigma
ti env (Exists (MExprDef _ args body)) = do
  tvs <- sequence $ newTyVar <$ args
  let idArgs = unpackIdArg <$> args

  -- Remove the function params from the env, aka "shadowing"
  let TypeEnv env' = foldl removeProgramVar env args
  -- New env, with the newly created var in env''
  let env'' = TypeEnv $ HM.union env' $ HM.fromList $ zip idArgs (Scheme [] <$> tvs)

  (subBody, tbody) <- tiBody env'' body

  let tres = case (length args, tbody) of
        (0, MFunZero _) -> tbody
        (0, _)          -> MFunZero tbody
        _               -> foldr MFun tbody (substitute subBody <$> tvs)

  pure (subBody, tres)
ti env (Exists (MExprCall _ fexpr params)) =
  foldl (tiAppFolder env) (ti env fexpr) params
  where
    tiAppFolder :: TypeEnv -> TI (Subst, MType) -> ExistsMExpr -> TI (Subst, MType)
    tiAppFolder env acc e2 = do
      (prevSub, prevType) <- acc
      let env' = substitute prevSub env
      (paramSub, paramType) <- ti env' e2
      tv <- newTyVar
      retSub <- unify (substitute paramSub prevType) (MFun paramType tv)
      pure (retSub `composeSubst` paramSub `composeSubst` prevSub, substitute retSub tv)
ti env (Exists (MExprIfElse _ ifCond [] elseBody)) = do
  (ifSub, ifType)      <- tiCondBlock env ifCond
  (elseSub, elseType) <- tiBody env elseBody

  retSub <- unify (substitute ifSub elseType) ifType
  
  pure (retSub `composeSubst` ifSub `composeSubst` elseSub, ifType)
ti _ expr = trace (show expr) undefined

findAssignments :: [ExistsMExpr] -> HM.HashMap String ExistsMExpr
findAssignments exprs = HM.fromList $ foldl' ass [] exprs
  -- TODO: Die for duplicate lhs
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
      (s, t) <- ti env expr

      pure (env, substitute s t : prev)

inferTopLevelDefs :: [ExistsMExpr] -> TI ()
inferTopLevelDefs exprs = do
  globalDefs     .= builtinTypes
  topLevelMExprs .= findAssignments exprs
  traverse_ inferAssignment exprs
  where
    exprs' = fixAss <$> exprs

    fixAss :: ExistsMExpr -> ExistsMExpr
    fixAss (Exists ass@MExprAssignment {_lhs, _rhs}) =
      Exists $ ass & rhs .~ fixExpr _lhs _rhs
    fixAss expr = expr

fixExpr :: Arg -> ExistsMExpr -> ExistsMExpr
fixExpr lhs expr =
  Exists $ MExprCall emptySp (Exists $ MExprId emptySp "fix") [innerFunc]
  where
    emptySp = SourcePos "" (mkPos maxBound) (mkPos maxBound)
    innerFunc = Exists $ MExprDef emptySp [lhs] [expr]

inferAssignment :: ExistsMExpr -> TI ()
inferAssignment (Exists MExprAssignment {_lhs, _rhs}) = do
  let lhsId = unpackIdArg _lhs
  previousDefs <- use globalDefs
  unless (HM.member lhsId previousDefs) $ do
    tv <- newTyVar
    let newEnv = TypeEnv (HM.singleton lhsId $ generalize emptyTypeEnv tv)
    (_, defType) <- ti newEnv _rhs
  
    let newDefs = [(lhsId, simplifyType defType)]
    globalDefs %= HM.union (HM.fromList newDefs)
inferAssignment _ = throwError "Top level exprs must be assignments"


-- Return Nothing when we should stop crawling (because we found a match!)
fixHelper :: Id -> ExistsMExpr -> Maybe Bool
fixHelper name e@(Exists MExprId { _id }) =
  if _id == name then
    Nothing
  else
    pure True
fixHelper name (Exists MExprDef {}) = pure False
fixHelper _ _                       = pure True

needsFix :: Id -> ExistsMExpr -> Bool
needsFix name mexpr =
  case crawl (fixHelper name) mexpr of
    Just _  -> False
    Nothing -> True
