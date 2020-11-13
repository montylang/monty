module MontyRunner where

import Debug.Trace
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Megaparsec
import System.Exit
import Data.List
import Control.Lens
import Data.IORef

import ParserTypes
import RunnerTypes
import CallableUtils
import RunnerUtils
import MorphUtils
import Evaluators.Evaluatable
import ModuleLoader
import Interop.Prelude

showCallStack :: [SourcePos] -> String
showCallStack positions = intercalate "\n" $
  ("    " <>) <$> sourcePosPretty <$> positions

run :: [ET] -> IO ()
run prog = do
    eContext <- emptyContext
    val <- evalStateT (runExceptT runCatch) eContext
    case val of
      Left (ErrString message) -> die message
      _                  -> pure ()
  where
    exitOnError :: ErrVal -> Scoper ()
    exitOnError (ErrString err) = do
      stack <- use callStack
      liftIO $ die $ "StackTrace: " <> err <> "\n" <> showCallStack stack

    runCatch :: Scoper ()
    runCatch = catchError (run' prog) exitOnError

    run' :: [ET] -> Scoper ()
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

emptyContext :: IO Runtime
emptyContext = do
    emptyBlock <- newIORef HM.empty
    pure $ Runtime HM.empty [emptyBlock] [] emptySp
  where
    emptySp :: SourcePos
    emptySp = SourcePos "" (mkPos maxBound) (mkPos maxBound)
