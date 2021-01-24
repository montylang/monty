module Interop.Tuple (tupleDefinitions) where

import Control.Monad.State.Strict
import Data.Foldable (foldrM)
import Data.Char (ord, chr)
import Data.List

import ParserTypes
import PrettyPrint
import TypeUtils
import RunnerTypes
import RunnerUtils
import CallableUtils

import Interop.Helpers

tupleStrImpl :: [Value] -> Scoper Value
tupleStrImpl [VTuple []] = do
  pure $ VList $ VChar <$> "()"
tupleStrImpl [VTuple xs] = do
    inner <- sequence $ strElement <$> xs
    pure $ VList $ [VChar '('] <>
      intercalate [VChar ',', VChar ' '] (lElements <$> inner)
      <> [VChar ')']
  where
    strElement :: Value -> Scoper Value
    strElement x = do
      impl <- findImplsInScope "str" x
      (evaluateImpl impl . pure) x

tupleDefinitions :: [(Id, Id, [FunctionCase])]
tupleDefinitions = [
    ("Tuple", "str", [
        generateInteropCase
          [TypedIdArg "value" "Tuple"]
          tupleStrImpl
    ])
  ]
