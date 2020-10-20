module Main where

import System.Environment
import Text.Megaparsec

import MontyRunner (run)
import Repl (runRepl)
import Parser.Root (rootBodyParser)

parseFromFile p file = runParser p file <$> readFile file

runFile :: String -> IO ()
runFile path = do
  parsedProgram <- parseFromFile rootBodyParser path
  case parsedProgram of
    Right prog -> run prog
    Left  err  -> (putStrLn . errorBundlePretty) err
  pure ()

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: monty [OPTION]... [FILE]"
  putStrLn "Examples:"
  putStrLn "monty --repl"
  putStrLn "monty foo.my"

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--repl"] -> runRepl
    [path]     -> runFile path
    _          -> showUsage
