{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module InteropPrelude (preludeDefinitions) where

import Control.Monad.State.Strict
import Data.Maybe

import ParserTypes
import RunnerTypes
import RunnerUtils
import MontyRunner

debugImpl :: [Value] -> Scoper Value
debugImpl [input] = do
  lift $ putStrLn $ show input
  pure input

consMapImpl :: [Value] -> Scoper Value
consMapImpl [x, (VList xs), func] = do
  ranHead <- runFun func [x]
  ranTail <- sequence $ (\el -> runFun func [el]) <$> xs
  pure $ VList (ranHead:ranTail)

nilMapImpl :: [Value] -> Scoper Value
nilMapImpl _ = pure $ VList []

consImpl :: [Value] -> Scoper Value
consImpl [cHead, (VList cTail)] = pure $ VList (cHead:cTail)

preludeDefinitions :: [(Id, Value)]
preludeDefinitions =
  [
    ("debug", VFunction [generateInteropCase [IdArg "value"] debugImpl]),
    -- TODO: Don't overrwrite this
    ("map", VTypeFunction "Functor" "map" ["self", "func"] [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"], IdArg "func"]
          consMapImpl,
        generateInteropCase
          [PatternArg "Nil" [], IdArg "func"]
          nilMapImpl
    ]),
    ("Nil", VFunction [generateInteropCase [] (const . pure $ VList [])]),
    ("Cons", VFunction [generateInteropCase [IdArg "head", IdArg "tail"] consImpl])
  ]
