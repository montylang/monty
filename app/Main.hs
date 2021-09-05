module Main where

import System.Environment
import Text.Megaparsec
import Control.Monad.Except

import MontyPrinter (run)
import Parser.Root (rootBodyParser)
import ModuleLoader (montyParseFromFile, montyRunSemantic)

runFile :: String -> IO ()
runFile path = do
  parsedProgram <- montyRunSemantic =<< montyParseFromFile path

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
    (path:_)   -> runFile path
    _          -> showUsage
