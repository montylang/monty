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

    emptyContext :: IO Context
    emptyContext = do
      emptyBlock <- newIORef HM.empty
      pure $ Context [emptyBlock] (Executors evaluateP evaluate) []

    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f ~(a, b, c) = f a b c

    addOrUpdateInterops :: Id -> Id -> [FunctionCase] -> Scoper ()
    addOrUpdateInterops cname name cases = do
      result <- findInTopScope name
      newInterops <- case result of
        Just a  -> foldM (flip $ addToStub cname) a cases
        Nothing -> do
          comb <- ahhhh cases
          pure $ VFunction $ FunctionImpl cases comb

      addToScope name newInterops

    ahhhh :: [FunctionCase] -> Scoper [Type]
    ahhhh cases = do
      (t:ts) <- sequence $ uhhhh <$> cases
      foldM combineTypes t ts

    uhhhh :: FunctionCase -> Scoper [Type]
    uhhhh c = sequence $ argToType <$> (fcaseArgs c)
