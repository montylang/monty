module ModuleLoader (loadModule, montyParseFromFile, toParseExcept) where

import System.Directory
import System.Exit
import System.FilePath
import Data.List
import Data.Void
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Megaparsec

import Evaluators.Evaluatable
import ParserTypes
import RunnerTypes
import RunnerUtils
import Parser.Root
import Parser.Semantic

loadModule :: [String] -> Scoper ()
loadModule components = do
    isFile <- liftIO $ doesFileExist (path <> ".my")
    isDir  <- liftIO $ doesDirectoryExist path

    if isFile then
      loadFiles [path <> ".my"]
    else if isDir then do
      content <- (liftIO $ listDirectory path)
      loadFiles $ constructPath <$> filter isMontyFile content
    else
      stackTrace $ "Could not find module " <> intercalate "." components
  where
    path = intercalate [pathSeparator] components
    constructPath = (<>) $ path <> [pathSeparator]
    isMontyFile = (== ".my") . takeExtension

loadFiles :: [FilePath] -> Scoper ()
loadFiles paths = do
    sequence_ (loadFile <$> paths)
    pure ()
  where
    -- evalPNotMain :: RExpr -> Scoper Value
    -- evalPNotMain (RExprAssignment _ (IdArg "__main__") _) = pure voidValue
    -- evalPNotMain other = eval other

    loadFile :: String -> Scoper ()
    loadFile path = do
      parsed <- liftIO $ montyParseFromFile path

      case runExcept parsed of
        Right exprs -> (sequence $ evaluate <$> exprs) *> pure ()
        Left err    -> liftIO $ die $ show err

toParseExcept :: Either (ParseErrorBundle String Void) a -> ParseExcept a
toParseExcept (Right a) = pure a
toParseExcept (Left err) = throwError $ ErrParse err

montyParseFromFile :: String -> IO (ParseExcept [ET])
montyParseFromFile file = do
  parsed <- toParseExcept <$> (runParser rootBodyParser file <$> readFile file)
  pure $ sequence =<< (semantic <$>) <$> parsed
