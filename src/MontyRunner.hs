module MontyRunner where

import Debug.Trace
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Megaparsec
import System.Exit
import Data.List
import Lens.Micro.Platform
import Data.IORef

import ParserTypes
import RunnerTypes
import CallableUtils
import RunnerUtils
import MorphUtils
import Evaluators.Assignment
import Evaluators.Condition
import Evaluators.Infix
import Evaluators.Types
import Evaluators.Case
import ModuleLoader
import Interop.Prelude

evaluate :: RExpr -> Scoper Value
evaluate (RExprId _ "_") = stackTrace "Cannot use black hole as variable"
evaluate (RExprId _ name) = do
  value <- findInScope name
  case value of
    Just val -> pure val
    Nothing  -> stackTrace (name <> " is not in scope")

evaluate (RExprClass _ name constructors) =
  evalClass name constructors

evaluate (RExprType _ name headers) =
  evalType name headers

evaluate (RExprInstanceOf _ className typeName implementations) =
  evalInstanceOf className typeName implementations

evaluate (RExprInt _ a) = pure $ VInt a
evaluate (RExprChar _ a) = pure $ VChar a

evaluate (RExprInfix _ first op second) = evalInfix first op second

evaluate (RExprIfElse _ ifCond elifConds elseBody) =
  evalCondition ifCond elifConds elseBody

evaluate (RExprCase _ input bodies) =
  evalCase input bodies

evaluate (RExprTuple _ values) =
  VTuple <$> (sequence $ eval <$> values)

evaluate (RExprList _ []) = pure $ VList []
evaluate (RExprList _ (x:xs)) = do
    headEvaled <- eval x
    tailEvaled <- sequence $ enforceType headEvaled <$> xs
    pure $ VList (headEvaled:tailEvaled)
  where
    enforceType :: Value -> RExpr -> Scoper Value
    enforceType headVal expr = do
      evaled <- eval expr
      assert (typesEqual evaled headVal) "List must be of the same type"
      pure evaled

evaluate (RExprDef _ args body) = do
  types    <- sequence $ argToType <$> args
  VScoped (VFunction $ FunctionImpl [FunctionCase args body] types) <$> use scope

evaluate (RExprAssignment _ dest value) =
  evalAssignment dest value

evaluate (RExprCall _ funExpr args) = do
    pushToCallStack funExpr
    fun        <- eval funExpr
    evaledArgs <- sequence $ eval <$> args
    runFun fun evaledArgs <* popFromCallStack
  where
    pushToCallStack :: RExpr -> Scoper ()
    pushToCallStack expr = callStack %= ((rpos expr):)

    popFromCallStack :: Scoper ()
    popFromCallStack = callStack %= (drop 1)

evaluate (RExprImport _ components) = do
  loadModule components
  pure voidValue

showCallStack :: [SourcePos] -> String
showCallStack positions = intercalate "\n" $
  ("    " <>) <$> sourcePosPretty <$> positions

run :: [RExpr] -> IO ()
run prog = do
    eContext <- emptyContext
    val <- runExceptT (evalStateT (run' prog) eContext)
    case val of
      Left (ErrString message) -> die message 
      _                        -> pure ()
  where
    run' :: [RExpr] -> Scoper ()
    run' exprs = do
      loadMyLib
      sequence_ $ eval <$> exprs

      mainEntry <- findInScope "__main__"
      sequence_ $ runIOVal <$> mainEntry

runIOVal :: Value -> Scoper ()
runIOVal (VTypeInstance "IO" "IO" [mainFun]) = do
    runFun mainFun [baseWorld] *> pure ()
  where
    baseWorld = VTypeInstance "#IOWorldToken" "#IOWorldToken" []
runIOVal v@(VInferred _ _ _) = do
  inferredIO <- applyInferredType "IO" v
  runIOVal inferredIO
runIOVal _ =
  stackTrace "Tried to bootstrap a non IO value."

loadMyLib :: Scoper ()
loadMyLib = do
    loadModule ["mylib", "prelude"]
    sequence_ $ (uncurry3 addOrUpdateInterops) <$> preludeDefinitions
    loadModule ["mylib", "postlude"]
  where
    addOrUpdateInterops :: Id -> Id -> [FunctionCase] -> Scoper ()
    addOrUpdateInterops cname name cases = do
      result <- findInTopScope name
      newInterops <- case result of
        Just a  -> foldM (flip $ addToStub cname) a cases
        Nothing -> do
          comb <- casesToTypes cases
          pure $ VFunction $ FunctionImpl cases comb

      replaceInScope name newInterops

    casesToTypes :: [FunctionCase] -> Scoper [Type]
    casesToTypes cases = do
      (t:ts) <- sequence $ caseToTypes <$> cases
      foldM combineTypes t ts

    caseToTypes :: FunctionCase -> Scoper [Type]
    caseToTypes c = sequence $ argToType <$> (fcaseArgs c)

evaluateCatch :: RExpr -> Scoper Value
evaluateCatch rexpr = catchError (evaluate rexpr) exitOnError
  where
    exitOnError :: ErrVal -> Scoper Value
    exitOnError (ErrString err) = do
      stack <- use callStack
      liftIO $ die $
        sourcePosPretty (rpos rexpr) <> ": " <> err <> "\n" <> showCallStack stack

emptyContext :: IO Context
emptyContext = do
  emptyBlock <- newIORef HM.empty
  pure $ Context HM.empty [emptyBlock] (Executors evaluateCatch) []
