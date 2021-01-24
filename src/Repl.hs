module Repl (runRepl) where

import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List
import Data.IORef
import System.Exit
import System.Console.Repline
import Text.Megaparsec
import Debug.Trace

import Evaluators.Evaluatable
import PrettyPrint
import ParserTypes
import RunnerTypes
import RunnerUtils
import MontyRunner (loadMyLib, showCallStack, runIOVal, emptyContext)
import Parser.Root (exprParser, importParser, runMyParser)
import Parser.Utils (ws)
import Parser.Semantic (semantic, ParseExcept)
import ModuleLoader (toParseExcept)

runRepl :: IO ()
runRepl = do
  eContext <- emptyContext
  val <- evalStateT (runExceptT repl) eContext
  case val of
    Left (ErrString message) -> die message 
    _                        -> pure ()

type Repl a = HaskelineT Scoper a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = lift runLine
  where
    runLine :: Scoper ()
    runLine = case runExcept parseInput of
      Right prog -> evaluatePrint prog *> pure ()
      Left  err  -> liftIO $ putStrLn $ show err

    parseInput :: ParseExcept ET
    parseInput = do
      parsed <- toParseExcept $ runMyParser replParser "repl" input
      semantic parsed

    replParser :: Parser PExpr
    replParser = ws *> (importParser <|> exprParser "") <* ws <* eof

evaluatePrint :: ET -> Scoper Value
evaluatePrint evalable =
    catchError (evalAndPrint evalable) printOnError *> pure unitValue
  where
    evalAndPrint :: ET -> Scoper ()
    evalAndPrint evalable = do
      res <- eval evalable

      case res of
        VTuple []                    -> pure () -- Don't echo void
        val@(VTypeInstance "IO" _ _) -> runIOVal val
        _                            -> liftIO $ putStrLn $ prettyPrint res
    
    printOnError :: ErrVal -> Scoper ()
    printOnError (ErrString err) = do
      stack <- use callStack
      callStack .= []
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
