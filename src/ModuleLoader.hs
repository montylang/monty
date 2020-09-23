module ModuleLoader (loadModule) where

import System.Directory
import System.FilePath
import Data.List
import Control.Monad.State.Strict
import Text.Megaparsec

import ParserTypes
import RunnerTypes
import RunnerUtils
import MontyParser (rootBodyParser)

loadModule :: (PExpr -> Scoper Value) -> [String] -> Scoper ()
loadModule evaler components = do
    isFile <- lift $ doesFileExist (path <> ".my")
    isDir  <- lift $ doesDirectoryExist path

    if isFile then
      loadFiles evaler [path <> ".my"]
    else if isDir then do
      content <- (lift $ listDirectory path)
      loadFiles evaler $ constructPath <$> filter isMontyFile content
    else
      runtimeError $ "Could not find module " <> intercalate "." components
  where
    path = intercalate [pathSeparator] components
    constructPath = (<>) $ path <> [pathSeparator]
    isMontyFile = (== ".my") . takeExtension

loadFiles :: (PExpr -> Scoper Value) -> [FilePath] -> Scoper ()
loadFiles evaler paths = do
    _ <- sequence (loadFile <$> paths)
    pure ()
  where
    parseFromFile p file = runParser p file <$> readFile file

    loadFile :: String -> Scoper ()
    loadFile path = do
      parsed <- lift $ parseFromFile rootBodyParser path
    
      case parsed of
        (Right exprs) -> (sequence $ evaler <$> exprs) *> pure () 
        (Left a) -> (runtimeError $ errorBundlePretty a) *> pure ()
