module MontyRunner where

import Prelude
import Debug.Trace
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Monad.State.Lazy

import MontyParser

data TypeClass
  = TAny
  | TInt
  | TString
  deriving (Show, Eq)

data ScopeKey
  = VariableKey String
  | FuncSigKey String [TypeClass]
  deriving (Show, Eq)

instance Hashable ScopeKey where
  hashWithSalt salt (VariableKey name) = hashWithSalt salt name

  hashWithSalt salt (FuncSigKey name types) =
    hashWithSalt salt (intercalate "," ([name] ++ (show <$> types)))

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

eval :: Expr -> Scoper Value
eval (ExprInt a) = pure $ VInt a
eval (ExprString a) = pure $ VString a
eval (ExprInfix first op second) = do
  f <- eval first
  s <- eval second
  pure $ infixEval f op s

eval (ExprCall (ExprId "debug") [param]) = do
  evaled <- eval param
  trace (show evaled) (pure evaled)

eval otherwise = trace ("Debug: " <> show otherwise) $ pure $ VString "Catch all. Much bad."

mapArgsToTypes :: [Arg] -> [TypeClass]
mapArgsToTypes args = (\_ -> TAny) <$> args

mergeScoper :: Scope -> Scoper ()
mergeScoper newValues = modify (\s -> HM.union s newValues)

loadFunctionsIntoScope :: [Expr] -> Scoper ()
loadFunctionsIntoScope exprs = mergeScoper newScopeEntries
  where
    newScopeEntries :: Scope
    newScopeEntries = HM.fromList [x | Just x <- (tuplify <$> exprs)] -- Decently epic

    tuplify :: Expr -> Maybe (ScopeKey, Value)
    tuplify (ExprAssignment name (ExprDef args body)) = 
      Just ((FuncSigKey name $ mapArgsToTypes args), (VFunction args body))
    tuplify _ = Nothing

runs :: [Expr] -> Scoper ()
runs exprs = do
  loadFunctionsIntoScope exprs
  _ <- sequence $ eval <$> exprs
  pure ()

run :: [Expr] -> IO ()
run exprs = evalStateT (runs exprs) HM.empty
