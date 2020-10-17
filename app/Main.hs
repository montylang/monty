module Main where

import System.Environment
import Text.Megaparsec

import MontyRunner (run)
import Parser.Root (rootBodyParser)

parseFromFile p file = runParser p file <$> readFile file

main :: IO ()
main = do
  args   <- getArgs
  -- FIXME: Opt parsing proprly
  parsedProgram <- parseFromFile rootBodyParser (head args)
  case parsedProgram of
    Right(prog) -> run prog
    Left(err)   -> (putStrLn . errorBundlePretty) err
  pure ()
