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
  case (ranHead, sequence ranTail) of
    (Right h, Right t) -> pure $ VList (h:t)
    (Left err, _)      -> stackTrace err
    (_, Left err)      -> stackTrace err

nilMapImpl :: [Value] -> Scoper Value
nilMapImpl _ = pure $ VList []

consImpl :: [Value] -> Scoper Value
consImpl [cHead, (VList cTail)] = pure $ VList (cHead:cTail)

preludeDefinitions :: [(Id, [FunctionCase])]
preludeDefinitions =
  [
    ("debug", [generateInteropCase [IdArg "value"] debugImpl]),
    ("map", [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"], IdArg "func"]
          consMapImpl,
        generateInteropCase
          [PatternArg "Nil" [], IdArg "func"]
          nilMapImpl
    ]),
    ("Nil", [generateInteropCase [] (const . pure $ VList [])]),
    ("Cons", [generateInteropCase [IdArg "head", IdArg "tail"] consImpl])
  ]
