module Interop.IO (ioDefinitions) where

import Control.Monad.State.Strict
import Data.Foldable (foldrM)
import Data.Char (ord, chr)

import ParserTypes
import PrettyPrint
import TypeUtils
import RunnerTypes
import RunnerUtils
import CallableUtils

import Interop.Helpers

strToValue :: String -> Value
strToValue s = VList $ VChar <$> s

ioPrintStrT :: [Value] -> Scoper Value
ioPrintStrT [VList str@((VChar _):_), token] = do
  liftIO $ putStrLn $ (vChr <$> str)
  pure $ VTuple [voidValue, token]

ioReadStrT :: [Value] -> Scoper Value
ioReadStrT [token] = do
  input <- strToValue <$> liftIO getLine
  pure $ VTuple [input, token]

ioDefinitions :: [(Id, Id, [FunctionCase])]
ioDefinitions = [
    ("Any", "printStrT", [
        generateInteropCase
          [TypedIdArg "input" "List",
           TypedIdArg "world" "#IOWorldToken"]
          ioPrintStrT
    ]),
    ("Any", "readStrT", [
        generateInteropCase
          [TypedIdArg "world" "#IOWorldToken"]
          ioReadStrT
    ])
  ]
