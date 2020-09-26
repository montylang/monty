module MontyRunner where

import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Megaparsec
import System.Exit

import ParserTypes
import RunnerTypes
import CallableUtils
import RunnerUtils
import Evaluators.All
import ModuleLoader

evaluateP :: PExpr -> Scoper Value
evaluateP (Pos pos expr) = catchError (eval expr) exitOnError
  where
    exitOnError :: ErrVal -> Scoper Value
    exitOnError (ErrString err) =
      liftIO $ die $ sourcePosPretty pos <> ": " <> err

evaluate :: Expr -> Scoper Value
evaluate (ExprId "_") = stackTrace "Cannot use black hole as variable"
evaluate (ExprId name) = do
    value <- findInScope name
    case toVScoped <$> value of
      Just val -> pure val
      Nothing  -> stackTrace (name <> " is not in scope")
  where
    toVScoped :: (Value, Scope) -> Value
    -- Only return associated scopes for functions
    toVScoped (VFunction cases, s) = VScoped (VFunction cases) s
    toVScoped (value, _) = value

evaluate (ExprClass name constructors) =
  evalClass name constructors

evaluate (ExprType name headers) =
  evalType name headers

evaluate (ExprInstanceOf className typeName implementations) =
  evalInstanceOf className typeName implementations

evaluate (ExprInt a) = pure $ VInt a
evaluate (ExprString a) = pure $ VString a

evaluate (ExprInfix first op second) = evalInfix first op second

evaluate (ExprIfElse ifCond elifConds elseBody) =
  evalCondition ifCond elifConds elseBody

evaluate (ExprList []) = pure $ VList []
evaluate (ExprList (x:xs)) = do
    headEvaled <- evalP x
    tailEvaled <- sequence $ enforceType headEvaled <$> xs
    pure $ VList (headEvaled:tailEvaled)
  where
    enforceType :: Value -> PExpr -> Scoper Value
    enforceType headVal expr = do
      evaled <- evalP expr
      assert (typesEqual evaled headVal) "List must be of the same type"
      pure evaled

evaluate (ExprDef args body) =
  pure $ VFunction [FunctionCase args body]

evaluate (ExprAssignment name value) = evalAssignment name value

evaluate (ExprCall funExpr args) = do
  fun        <- evalP funExpr
  evaledArgs <- sequence $ evalP <$> args
  runFun fun evaledArgs

evaluate (ExprUnwrap content) = evalUnwrap content

evaluate (ExprImport components) = do
  loadModule components
  pure $ VInt 0

evaluate other = stackTrace ("Error (unimplemented expr evaluate): " <> show other)
