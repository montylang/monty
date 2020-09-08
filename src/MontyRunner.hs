module MontyRunner where

import Prelude
import Debug.Trace
import Data.List (intercalate)
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Monad.State.Strict

import MontyParser

data TypeClass
  = TAny
  | TInt
  | TString
  deriving (Show, Eq)

-- TODO: Scope blocks - List of scopes

data ScopeKey
  = VariableKey String
  -- | FuncSigKey String [TypeClass]
  deriving (Show, Eq)

instance Hashable ScopeKey where
  hashWithSalt salt (VariableKey name) = hashWithSalt salt name

  -- hashWithSalt salt (FuncSigKey name types) =
  --   hashWithSalt salt (intercalate "," ([name] ++ (show <$> types)))

type Scope = HM.HashMap ScopeKey Value
type Scoper a = StateT Scope IO a

data Value
  = VInt Int
  | VString String
  | VBoolean Bool
  | VFunction [Arg] [Expr] -- Parametric
  | VList     -- Parametric
  | VTuple    -- Parametric
  | VDict     -- Parametric
  | VClass    -- Parametric
  deriving (Show)

infixEval :: Value -> InfixOp -> Value -> Value
infixEval (VInt first) InfixAdd (VInt second) = VInt $ first + second
infixEval (VInt first) InfixMul (VInt second) = VInt $ first * second

ecart :: String -> a -> Scoper a
ecart msg rest = lift $ putStrLn msg *> pure rest

showAScope :: String -> Scope -> Scoper ()
showAScope prefix scope = do
  _ <- lift $ putStrLn (prefix <> ": " <> (intercalate ", " ((\(VariableKey a) -> a) <$> HM.keys scope)))
  pure ()

showScope :: String -> Scoper ()
showScope prefix = do
  s <- get
  showAScope prefix s

eval :: Expr -> Scoper Value
eval (ExprId name) = do
  -- showScope $ "id (" <> name <> ")"
  value <- gets (\s -> HM.lookup (VariableKey name) s)
  pure $ fromMaybe (trace (name <> " is not in scope") undefined) value -- TODO: Have a better message

eval (ExprInt a) = pure $ VInt a
eval (ExprString a) = pure $ VString a
eval (ExprInfix first op second) = do
  f <- eval first
  s <- eval second
  pure $ infixEval f op s

eval (ExprDef args body) = pure $ VFunction args body

eval (ExprAssignment name value) = do
  evaledValue <- eval value
  modify (\s -> HM.insert (VariableKey name) evaledValue s)
  pure evaledValue

eval (ExprCall (ExprId "debug") [param]) = do
  evaled <- eval param
  _ <- lift $ (putStrLn $ show evaled)
  pure evaled

eval (ExprCall funExpr args) = do
    fun        <- eval funExpr
    evaledArgs <- sequence $ eval <$> args
    result     <- runFun fun evaledArgs
    pure result
  where
    runFun :: Value -> [Value] -> Scoper Value
    runFun (VFunction fargs body) params | (length args) == (length params) = do
      s <- get
      put $ HM.union (HM.fromList $ zip (convertArg <$> fargs) params) s
      runBody body
    -- FIXME: complete hack
    runFun _ _ = trace ("Error: Bad function call on line TODO") undefined

    runBody :: [Expr] -> Scoper Value
    runBody exprs = do
        -- showScope "run body"
        _ <- sequence $ eval <$> beginning
        eval returnExpr
      where 
        (beginning, returnExpr) = splitReturn exprs

    splitReturn :: [Expr] -> ([Expr], Expr)
    splitReturn exprs =
      let (beginning, [ExprReturn returnExpr]) = splitAt ((length exprs) - 1) exprs in
        (beginning, returnExpr)

    convertArg :: Arg -> ScopeKey
    convertArg (IdArg name) = VariableKey name

eval other = trace ("Debug: " <> show other) $ pure $ VString "Catch all. Much bad."

mapArgsToTypes :: [Arg] -> [TypeClass]
mapArgsToTypes args = (\_ -> TAny) <$> args

mergeScoper :: Scope -> Scoper ()
mergeScoper newValues = modify (\s -> HM.union s newValues)

runs :: [Expr] -> Scoper ()
runs exprs = do
  _ <- sequence $ eval <$> exprs
  pure ()

run :: [Expr] -> IO ()
run exprs = evalStateT (runs exprs) HM.empty
