module Main where

import System.Environment
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec

import InteropPrelude
import RunnerTypes
import RunnerUtils (addToScope, findInTopScope, addToStub)
import ParserTypes
import MontyRunner (evalP)
import MontyParser (rootBodyParser)
import ModuleLoader

-- TODO: This entire file is waste

parseFromFile p file = runParser p file <$> readFile file

run :: [PExpr] -> Scoper ()
run exprs = do
    _ <- loadModule evalP ["mylib", "prelude"]
    _ <- sequence $ (uncurry3 addOrUpdateInterops) <$> preludeDefinitions
    _ <- loadModule evalP ["mylib", "postlude"]
    _ <- sequence $ evalP <$> exprs
    pure ()
  where
    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f ~(a, b, c) = f a b c

    addOrUpdateInterops :: Id -> Id -> [FunctionCase] -> Scoper ()
    addOrUpdateInterops cname name body = do
      result <- findInTopScope name
      addToScope name $ case result of
        Just a  -> foldl (flip $ addToStub cname) a body
        Nothing -> VFunction body

lineSep :: String
lineSep = '-' <$ [1..80]

main :: IO ()
main = do
  args   <- getArgs
  -- FIXME: You know what's wrong ;)
  parsedProgram <- parseFromFile rootBodyParser (head args)
  putStrLn lineSep
  case parsedProgram of
    -- TODO: Show errors?
    (Right prog) -> (runExceptT $ evalStateT (run prog) [HM.empty]) *> pure ()
    (Left a) -> putStrLn $ errorBundlePretty a
  putStrLn lineSep
  pure ()
