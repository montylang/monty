module MontyRunner where

import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Megaparsec
import System.Exit
import Data.List
import Lens.Micro.Platform

import ParserTypes
import RunnerTypes
import CallableUtils
import RunnerUtils
import Evaluators.Unwrap
import Evaluators.Assignment
import Evaluators.Condition
import Evaluators.Infix
import Evaluators.Types
import ModuleLoader
import InteropPrelude

evaluateP :: PExpr -> Scoper Value
evaluateP (Pos pos expr) = catchError (eval expr) exitOnError
  where
    exitOnError :: ErrVal -> Scoper Value
    exitOnError (ErrString err) = do
      stack <- use callStack
      liftIO $ die $
        sourcePosPretty pos <> ": " <> err <> "\n" <> showCallStack stack

evaluate :: Expr -> Scoper Value
evaluate (ExprId "_") = stackTrace "Cannot use black hole as variable"
evaluate (ExprId name) = do
  value <- findInScope name
  case value of
    Just val -> pure $ view _1 val
    Nothing  -> stackTrace (name <> " is not in scope")

evaluate (ExprClass name constructors) =
  evalClass name constructors

evaluate (ExprType name headers) =
  evalType name headers

evaluate (ExprInstanceOf className typeName implementations) =
  evalInstanceOf className typeName implementations

evaluate (ExprInt a) = pure $ VInt a
evaluate (ExprChar a) = pure $ VChar a

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

evaluate (ExprDef args body) = do
    types <- sequence $ argToType <$> args
    VScoped (VFunction $ FunctionImpl [FunctionCase args body] types) <$> use scope

evaluate (ExprAssignment name value) =
    evalAssignment name value <* (scope %= rescope)
  where
    rescope :: Scope -> Scope
    rescope vals = vals & (ix 0) %~ (HM.map . updateScoped) vals

    updateScoped :: Scope -> Value -> Value
    updateScoped s (VScoped v _) = VScoped v s
    updateScoped _ v = v

evaluate (ExprCall funExpr args) = do
    pushToCallStack funExpr
    fun        <- evalP funExpr
    evaledArgs <- sequence $ evalP <$> args
    runFun fun evaledArgs <* popFromCallStack
  where
    pushToCallStack :: PExpr -> Scoper ()
    pushToCallStack (Pos p _) = callStack %= (p:)

    popFromCallStack :: Scoper ()
    popFromCallStack = callStack %= (drop 1)

evaluate (ExprUnwrap content) = evalUnwrap content

evaluate (ExprImport components) = do
  loadModule components
  pure $ VInt 0

evaluate other = stackTrace ("Error (unimplemented expr evaluate): " <> show other)

showCallStack :: [SourcePos] -> String
showCallStack positions = intercalate "\n" $
  ("    " <>) <$> sourcePosPretty <$> positions

run :: [PExpr] -> IO ()
run prog = do
    val <- runExceptT (evalStateT (run' prog) emptyContext)
    case val of
      Left (ErrString message) -> die message 
      _                        -> pure ()
  where
    exitOnError :: ErrVal -> Scoper ()
    exitOnError (ErrString err) = do
      stack <- use callStack
      liftIO $ die $ "Err: " <> err <> "\n" <> showCallStack stack
    
    run' :: [PExpr] -> Scoper ()
    run' exprs = do
      _ <- loadModule ["mylib", "prelude"]
      sequence_ $ (uncurry3 addOrUpdateInterops) <$> preludeDefinitions
      _ <- loadModule ["mylib", "postlude"]
      sequence_ $ evalP <$> exprs
      pure ()

    emptyContext :: Context
    emptyContext = Context [HM.empty] (Executors evaluateP evaluate) []

    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f ~(a, b, c) = f a b c

    addOrUpdateInterops :: Id -> Id -> [FunctionCase] -> Scoper ()
    addOrUpdateInterops cname name body = do
      result <- findInTopScope name
      newInterops <- case result of
        Just a  -> foldM (flip $ addToStub cname) a body
        -- TODO: Get types for impl
        Nothing -> pure $ VFunction $ FunctionImpl body []

      addToScope name newInterops
