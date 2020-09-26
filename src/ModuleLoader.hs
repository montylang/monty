module ModuleLoader (loadModule) where

import System.Directory
import System.Exit
import System.FilePath
import Data.List
import Control.Monad.State.Strict
import Text.Megaparsec

import RunnerTypes
import RunnerUtils
import MontyParser (rootBodyParser)

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
    _ <- sequence (loadFile <$> paths)
    pure ()
  where
    parseFromFile p file = runParser p file <$> readFile file

    loadFile :: String -> Scoper ()
    loadFile path = do
      parsed <- liftIO $ parseFromFile rootBodyParser path

      case parsed of
        (Right exprs) -> (sequence $ evalP <$> exprs) *> pure () 
        (Left a) -> liftIO $ die $ errorBundlePretty a
