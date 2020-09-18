module Main where

import System.Environment
import Text.Parsec.String
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as HM

import InteropPrelude
import RunnerTypes
import RunnerUtils (addToScope, findInTopScope, addToStub)
import ParserTypes
import MontyRunner (evalP) 
import MontyParser (rootBodyParser)

-- TODO: This entire file is waste

runs :: [PExpr] -> [PExpr] -> [PExpr] -> Scoper ()
runs preExprs postExprs exprs = do
    _ <- sequence $ evalP <$> preExprs
    _ <- sequence $ (uncurry addOrUpdateInterops) <$> preludeDefinitions 
    _ <- sequence $ evalP <$> postExprs
    _ <- sequence $ evalP <$> exprs
    pure ()
  where
    addOrUpdateInterops :: Id -> [FunctionCase] -> Scoper ()
    addOrUpdateInterops name body = do
      result <- findInTopScope name
      addToScope name $ case result of
        Just a  -> foldl (flip addToStub) a body
        Nothing -> VFunction body

run :: [PExpr] -> [PExpr] -> [PExpr] -> IO ()
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
