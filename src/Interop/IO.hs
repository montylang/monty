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

ioPrintUnsafe :: [Value] -> Scoper Value
ioPrintUnsafe [VList str@((VChar _):_)] = do
  liftIO $ putStrLn $ (vChr <$> str)
  pure voidValue

ioInputUnsafe :: [Value] -> Scoper Value
ioInputUnsafe [] = strToValue <$> liftIO getLine

ioDefinitions :: [(Id, Id, [FunctionCase])]
ioDefinitions = [
    ("Any", "scaryUnsafePrint", [
        generateInteropCase
          [TypedIdArg "input" "List"]
          ioPrintUnsafe
    ]),
    ("Any", "scaryUnsafeInput", [
        generateInteropCase
          []
          ioInputUnsafe
    ])
  ]
