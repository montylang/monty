module Repl (runRepl) where

import qualified Data.HashMap.Strict as HM
import Lens.Micro.Platform
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List
import Data.IORef
import System.Exit
import System.Console.Repline
import Text.Megaparsec
import Debug.Trace

import PrettyPrint
import ParserTypes
import RunnerTypes
import RunnerUtils
import MontyRunner (loadMyLib, showCallStack, evaluate)
import Parser.Root (exprParser, importParser)
import Parser.Utils (ws)

emptyContext :: IO Context
emptyContext = do
  emptyBlock <- newIORef HM.empty
  pure $ Context [emptyBlock] (Executors evaluateR evaluate) []

runRepl :: IO ()
runRepl = do
  eContext <- emptyContext
  val <- runExceptT (evalStateT repl eContext)
  case val of
    Left (ErrString message) -> die message 
    _                        -> pure ()

type Repl a = HaskelineT Scoper a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = lift runLine
  where
    runLine :: Scoper ()
    runLine = case runParser replParser "repl" input of
      Right prog -> evaluatePrint prog *> pure ()
      Left  err  -> liftIO $ (putStrLn . errorBundlePretty) err

    replParser :: Parser PExpr
    replParser = ws *> (importParser <|> exprParser "") <* ws

evaluateR :: PExpr -> Scoper Value
evaluateR (Pos pos expr) = eval expr

evaluatePrint :: PExpr -> Scoper Value
evaluatePrint (Pos pos expr) =
    catchError (evalAndPrint expr) printOnError *> pure voidValue 
  where
    evalAndPrint :: Expr -> Scoper ()
    evalAndPrint expr = do
      res <- eval expr

      case res of
        VTuple [] -> pure () -- Don't echo void
        nonVoid   -> liftIO $ putStrLn $ prettyPrint res
    
    printOnError :: ErrVal -> Scoper ()
    printOnError (ErrString err) = do
      stack <- use callStack
      liftIO $ putStrLn $ err <> "\n" <> showCallStack stack

-- Tab Completion: return a completion for partial words entered
completer :: String -> Scoper [String]
completer n = filter (isPrefixOf n) <$> getNames 
  where
    getNames :: Scoper [String]
    getNames = do
      s        <- use scope
      unreffed <- liftIO $ sequence $ readIORef <$> s
      pure $ unreffed >>= HM.keys

reload :: String -> Repl ()
reload _ = do
  liftIO $ putStrLn "Reloading..."

  emptyBlock <- liftIO $ newIORef HM.empty
  lift $ scope %= const [emptyBlock]

  lift loadMyLib
  pure ()

opts :: [(String, String -> Repl ())]
opts = [
  ("r", reload)
 ]

initialize :: Repl ()
initialize = do
  liftIO $ putStrLn "Welcome to the Monty repl!"
  lift loadMyLib

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  pure Exit

customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure ">>> "
customBanner MultiLine = pure "| "

repl :: Scoper ()
repl = evalReplOpts $ ReplOpts
  { banner           = customBanner
  , command          = cmd
  , options          = opts
  , prefix           = Just ':'
  , multilineCommand = Just "paste"
  , tabComplete      = (Word0 completer)
  , initialiser      = initialize
  , finaliser        = final
  }
