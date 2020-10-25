module Main where

import System.Environment
import Text.Megaparsec
import Control.Monad.Except

import MontyRunner (run)
import Repl (runRepl)
import Parser.Root (rootBodyParser)
import ModuleLoader (montyParseFromFile)

runFile :: String -> IO ()
runFile path = do
  parsedProgram <- montyParseFromFile path

  case runExcept parsedProgram of
    (Right prog) -> run prog
    (Left err) -> putStrLn $ show err
  
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
