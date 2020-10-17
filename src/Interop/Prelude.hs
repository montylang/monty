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
import Interop.Char
import Interop.IO

debugImpl :: [Value] -> Scoper Value
debugImpl [input] = do
  liftIO $ putStrLn $ prettyPrint input
  pure input

miscDefinitions :: [(Id, Id, [FunctionCase])]
miscDefinitions = [
    ("Any", "debug", [generateInteropCase [IdArg "value"] debugImpl])
  ]

preludeDefinitions :: [(Id, Id, [FunctionCase])]
preludeDefinitions =
  listDefinitions <>
  intDefinitions <>
  charDefinitions <>
  miscDefinitions <>
  ioDefinitions
