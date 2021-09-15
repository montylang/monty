{-# LANGUAGE DataKinds #-}
module MontyPrinter where

import MyPrelude

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import PrettyPrint
import Control.Monad.State
import ParserTypes (Id, Arg (IdArg, _idArgVal))
import Control.Monad.Except
import MorphUtils
import MiddleEndTypes
import Debug.Trace (trace)

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

instance Types Scheme where
    ftv (Scheme vars t) = HS.difference (ftv t) (HS.fromList vars)
    substitute s (Scheme vars t) = Scheme vars (substitute (foldr HM.delete s vars) t)

instance Types MType where
    ftv MInt         = HS.empty
    ftv (MVar name)     = HS.singleton name
    ftv (MFun inputTypes outputType) =
      HS.unions $ ftv <$> outputType:inputTypes

    substitute s (MVar name) = case HM.lookup name s of
                         Nothing  -> MVar name
                         Just t   -> t
    substitute s (MFun t1 t2) = MFun (substitute s <$> t1) (substitute s t2)
    substitute s t            = t

-- Creating a new polymorphic type var, a, b, ... a1, b1, etc.
newTyVar :: TI MType
newTyVar = do
  s <- get
  put $ s + 1
  pure $ MVar $ reverse $ toTyVar s
  where 
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
  return $ substitute s t

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
  (s2, t2) <- tiDefBody (substitute rhsSubst env'') body
  return (composeSubst rhsSubst s2, t2)

tiDefBody :: TypeEnv -> [ExistsMExpr] -> TI (Subst, MType)
tiDefBody env []  = throwError "Empty body"
tiDefBody env [x] = ti env x
tiDefBody env ((Exists (MExprAssignment _ lhs rhs):xs)) =
  tiLet env lhs rhs xs
tiDefBody env (_:xs) = tiDefBody env xs

-- Type inferrence
ti :: TypeEnv -> ExistsMExpr -> TI (Subst, MType)
ti (TypeEnv env) (Exists (MExprId _ name)) =
  case HM.lookup name env of
    Nothing    -> throwError $ "unbound variable: " ++ name
    Just sigma -> do
      t <- instantiate sigma
      return (nullSubst, t)
ti env (Exists (MExprDef _ args body)) = do
  tvs <- sequence $ newTyVar <$ args
  let idArgs = unpackIdArg <$> args

  -- Remove the function params from the env, aka "shadowing"
  let TypeEnv env' = foldl removeProgramVar env args
  -- New env, with the newly created var in env''
  let env'' = TypeEnv $ HM.union env' $ HM.fromList $ zip idArgs (Scheme [] <$> tvs)

  -- FIXME: Fix convert assignments to lets (basically)
  (s1, t1) <- ti env'' $ head body

  pure (s1, MFun (substitute s1 <$> tvs) t1)

ti _ _ = pure (HM.empty, MInt)

bootstrap :: HM.HashMap String Scheme -> ExistsMExpr -> TI MType
bootstrap env expr = do
  (s, t) <- ti (TypeEnv env) expr
  pure $ substitute s t

inferMeDaddy :: ExistsMExpr -> IO ()
inferMeDaddy expr = do
  let (res, _) = runTI (bootstrap HM.empty expr)
  case res of
    Left err -> putStrLn $ show expr ++ "\nerror: " ++ err
    Right t  -> putStrLn $ show expr ++ "\n:: " ++ show t

run :: [ExistsMExpr] -> IO ()
run prog = sequence_ $ inferMeDaddy <$> prog
