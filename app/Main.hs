module Main where

import System.Environment
import Text.Megaparsec
import Data.Bifunctor

import MontyRunner (run)
import MontyParser (rootBodyParser)

parseFromFile p file = runParser p file <$> readFile file

lineSep :: String
lineSep = '-' <$ [1..80]

main :: IO ()
main = do
  args   <- getArgs
  -- FIXME: Opt parsing proprly
  parsedProgram <- parseFromFile rootBodyParser (head args)
  putStrLn lineSep
  _ <- sequence $ bimap (putStrLn . errorBundlePretty) run parsedProgram
  putStrLn lineSep
  pure ()
