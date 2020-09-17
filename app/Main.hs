module Main where

import System.Environment
import Text.Parsec.String
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as HM
import Data.Either

import InteropPrelude
import RunnerTypes
import RunnerUtils (addToScope)
import ParserTypes
import MontyRunner (eval)
import MontyParser (rootBodyParser)

-- TODO: This entire file is waste

runs :: [Expr] -> [Expr] -> [Expr] -> Scoper ()
runs preExprs postExprs exprs = do
  _ <- sequence $ eval <$> preExprs
  _ <- sequence $ (uncurry addToScope) <$> preludeDefinitions
  _ <- sequence $ eval <$> postExprs
  _ <- sequence $ eval <$> exprs
  pure ()

run :: [Expr] -> [Expr] -> [Expr] -> IO ()
run preExprs postExprs exprs = evalStateT (runs preExprs postExprs exprs) [HM.empty]

lineSep :: String
lineSep = '-' <$ [1..80]

main :: IO ()
main = do
  args   <- getArgs
  -- FIXME: You know what's wrong ;)
  parsedPrelude <- parseFromFile rootBodyParser "mylib/prelude.my"
  parsedPostlude <- parseFromFile rootBodyParser "mylib/postlude.my"
  parsedProgram <- parseFromFile rootBodyParser (head args)
  putStrLn lineSep
  parsed <- pure $ do
    pre <- parsedPrelude
    post <- parsedPostlude
    program <- parsedProgram
    pure (pre, post, program)
  case parsed of
    (Right (pre, post, prog)) -> run pre post prog
    (Left a) -> putStrLn $ show a
  putStrLn lineSep
  pure ()
