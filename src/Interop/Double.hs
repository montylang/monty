module Interop.Double (doubleDefinitions) where

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

doubleCompareImpl :: [Value] -> Scoper Value
doubleCompareImpl [(VDouble first), (VDouble second)] =
  pure $ ordToVal $ compare first second

doubleStrImpl :: [Value] -> Scoper Value
doubleStrImpl [(VDouble value)] = pure $ VList $ VChar <$> show value

doubleDefinitions :: [(Id, Id, [FunctionCase])]
doubleDefinitions = [
    ("Double", "compare", [
        generateInteropCase
          [TypedIdArg "first" "Double", TypedIdArg "second" "Double"]
          doubleCompareImpl
    ]),
    ("Double", "str", [
        generateInteropCase
          [TypedIdArg "value" "Double"]
          doubleStrImpl
    ])
  ]
