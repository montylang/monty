module Main where

import System.Environment
import Text.Parsec
import Text.Parsec.String
import Data.Char

import MontyRunner
import MontyParser

lineSep :: String
lineSep = (\_ -> '-') <$> [1..80]

main :: IO ()
main = do
  args   <- getArgs
  parsed <- parseFromFile rootBodyParser (head args)
  putStrLn lineSep
  case parsed of
    Left a  -> putStrLn $ show a
    Right a -> run a
  putStrLn lineSep
  pure ()
