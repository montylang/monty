module Main where

import System.Environment
import Text.Parsec
import Text.Parsec.String
import Data.Char

import PyParser

lineSep :: String
lineSep = (\_ -> '-') <$> [1..80]

main :: IO ()
main = do
  putStrLn "hello"
  args   <- getArgs
  parsed <- parseFromFile rootBodyParser (head args)
  putStrLn lineSep
  putStrLn $ case parsed of
    Left a -> show a
    Right a -> show a
  putStrLn lineSep
  pure ()
