module Interop.Int (intDefinitions) where

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

intCompareImpl :: [Value] -> Scoper Value
intCompareImpl [(VInt first), (VInt second)] =
  pure $ ordToVal $ compare first second

intStrImpl :: [Value] -> Scoper Value
intStrImpl [(VInt value)] = pure $ VList $ VChar <$> show value

intChrImpl :: [Value] -> Scoper Value
intChrImpl [(VInt val)] = pure $ VChar $ chr val

intIntImpl :: [Value] -> Scoper Value
intIntImpl [(VList vals)] = pure $ VInt $ read $ toStr vals
  where
    toStr :: [Value] -> String
    toStr vals = vChr <$> vals

intDefinitions :: [(Id, Id, [FunctionCase])]
intDefinitions = [
    ("Int", "compare", [
        generateInteropCase
          [TypedIdArg "first" "Int", TypedIdArg "second" "Int"]
          intCompareImpl
    ]),
    ("Int", "str", [
        generateInteropCase
          [TypedIdArg "value" "Int"]
          intStrImpl
    ]),
    ("Int", "chr", [
        generateInteropCase
          [TypedIdArg "value" "Int"]
          intChrImpl
    ]),
    ("List", "int", [
        generateInteropCase
          [TypedIdArg "value" "List"]
          intIntImpl
    ])
  ]
