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
import Evaluators.Unwrap
import Evaluators.Assignment
import Evaluators.Condition
import Evaluators.Infix
import Evaluators.Types
import ModuleLoader
import Interop.Prelude

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
    Just val -> pure val
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

evaluate (ExprTuple values) =
  VTuple <$> (sequence $ evalP <$> values)

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
  types    <- sequence $ argToType <$> args
  VScoped (VFunction $ FunctionImpl [FunctionCase args body] types) <$> use scope
      

evaluate (ExprAssignment name value) =
  evalAssignment name value

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
    eContext <- emptyContext
    val <- runExceptT (evalStateT (run' prog) eContext)
    case val of
      Left (ErrString message) -> die message 
      _                        -> pure ()
  where
    run' :: [PExpr] -> Scoper ()
    run' exprs = do
      loadMyLib
      sequence_ $ evalP <$> exprs

      mainEntry <- findInScope "__main__"
      sequence_ $ runMain <$> mainEntry

    baseWorld = VTypeInstance "WorldClass" "World" []

    runMain :: Value -> Scoper ()
    runMain (VTypeInstance "IO" "IOVal" [mainFun]) = do
      runFun mainFun [baseWorld] *> pure ()
    runMain _ =
      stackTrace "Read the docs. This isn't what main is buddy"

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

      addToScope name newInterops

    casesToTypes :: [FunctionCase] -> Scoper [Type]
    casesToTypes cases = do
      (t:ts) <- sequence $ caseToTypes <$> cases
      foldM combineTypes t ts

    caseToTypes :: FunctionCase -> Scoper [Type]
    caseToTypes c = sequence $ argToType <$> (fcaseArgs c)

emptyContext :: IO Context
emptyContext = do
  emptyBlock <- newIORef HM.empty
  pure $ Context [emptyBlock] (Executors evaluateP evaluate) []
