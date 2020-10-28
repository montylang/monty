{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interop.Prelude (preludeDefinitions) where

import Control.Monad.State.Strict

import ParserTypes
import PrettyPrint
import TypeUtils
import RunnerTypes
import RunnerUtils
import CallableUtils

import Interop.List
import Interop.Int
import Interop.Double
import Interop.Char
import Interop.IO

debugImpl :: [Value] -> Scoper Value
debugImpl [input] = do
  liftIO $ putStrLn $ prettyPrint input
  pure input

passImpl :: [Value] -> Scoper Value
passImpl [] = stackTrace "Unimplemented branch point"

miscDefinitions :: [(Id, Id, [FunctionCase])]
miscDefinitions = [
    ("Any", "debug", [generateInteropCase [IdArg "value"] debugImpl]),
    ("Any", "pass", [generateInteropCase [] passImpl])
  ]

preludeDefinitions :: [(Id, Id, [FunctionCase])]
preludeDefinitions =
  listDefinitions <>
  intDefinitions <>
  doubleDefinitions <>
  charDefinitions <>
  miscDefinitions <>
  ioDefinitions
