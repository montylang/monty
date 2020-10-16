module Interop.Char (charDefinitions) where

import Data.Char (ord, chr)

import RunnerTypes
import ParserTypes
import PrettyPrint
import TypeUtils
import RunnerUtils
import CallableUtils

import Interop.Helpers

charCompareImpl :: [Value] -> Scoper Value
charCompareImpl [(VChar first), (VChar second)] =
  pure $ ordToVal $ compare first second

charStrImpl :: [Value] -> Scoper Value
charStrImpl v@[(VChar _)] = pure $ VList v

charEqualsImpl :: [Value] -> Scoper Value
charEqualsImpl [(VChar first), (VChar second)] =
  pure $ toBoolValue (first == second)

charOrdImpl :: [Value] -> Scoper Value
charOrdImpl [(VChar val)] = pure $ VInt $ ord val

charDefinitions :: [(Id, Id, [FunctionCase])]
charDefinitions = [
    ("Char", "compare", [
        generateInteropCase
          [TypedIdArg "first" "Char", TypedIdArg "second" "Char"]
          charCompareImpl
    ]),
    ("Char", "str", [
        generateInteropCase
          [TypedIdArg "value" "Char"]
          charStrImpl
    ]),
    ("Char", "equals", [
        generateInteropCase
          [TypedIdArg "first" "Char", TypedIdArg "second" "Char"]
          charEqualsImpl
    ]),
    ("Char", "ord", [
        generateInteropCase
          [TypedIdArg "val" "Char"]
          charOrdImpl
    ])
  ]

