{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module InteropPrelude where

import Control.Monad.State.Strict

import ParserTypes
import RunnerTypes
import RunnerUtils

debugImpl :: [Value] -> Scoper Value
debugImpl [input] = do
  lift $ putStrLn $ show input
  pure input

preludeDefinitions :: [(Id, Value)]
preludeDefinitions =
  [
    ("debug", VFunction [generateInteropCase [IdArg "value"] debugImpl])
  ]
