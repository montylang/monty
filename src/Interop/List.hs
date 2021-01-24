module Interop.List (listDefinitions) where

import Control.Monad.State.Strict

import ParserTypes
import PrettyPrint
import TypeUtils
import RunnerTypes
import RunnerUtils
import CallableUtils
import Data.Foldable (foldrM)
import Data.Char (ord, chr)
import Data.List

consMapImpl :: [Value] -> Scoper Value
consMapImpl [x, VList xs, func] = do
    ranHead <- runFun func [x]
    ranTail <- sequence $ ree ranHead <$> xs
    pure $ VList (ranHead:ranTail)
  where
    ree :: Value -> Value -> Scoper Value
    ree headVal el = do
      ranEl <- runFun func [el]
      if typesEqual headVal ranEl
        then pure ranEl
        else stackTrace $
            "A mapped function must always return the same type" <>
            show headVal <> "," <> show ranEl

nilMapImpl :: [Value] -> Scoper Value
nilMapImpl _ = pure $ VList []

consFoldlImpl :: [Value] -> Scoper Value
consFoldlImpl [x, VList xs, initial, folder] =
    foldM nativeFolder initial (x:xs)
  where
    nativeFolder :: Value -> Value -> Scoper Value
    nativeFolder acc it = runFun folder [acc, it]

nilFoldlImpl :: [Value] -> Scoper Value
nilFoldlImpl [initial, _] = pure initial

consFoldrImpl :: [Value] -> Scoper Value
consFoldrImpl [x, VList xs, initial, folder] =
    foldrM nativeFolder initial (x:xs)
  where
    nativeFolder :: Value -> Value -> Scoper Value
    nativeFolder it acc = runFun folder [it, acc]

nilFoldrImpl :: [Value] -> Scoper Value
nilFoldrImpl [initial, _] = pure initial

consBindImpl :: [Value] -> Scoper Value
consBindImpl [consHead, consTail, func] = do
    mapped <- consMapImpl [consHead, consTail, func]
    case mapped of
      VList values -> joinValues values
      _            -> stackTrace "Result of map in bind wasn't a list"
  where
    unList :: Value -> Scoper [Value]
    unList (VList vals)               = pure vals
    unList (VInferred "wrap" _ [val]) = pure [val]
    unList _ = stackTrace "Result of bind on list must be a list"

    joinValues :: [Value] -> Scoper Value
    joinValues values = do
      vals <- sequence $ unList <$> values
      pure $ VList $ join vals

nilBindImpl :: [Value] -> Scoper Value
nilBindImpl [_] = pure $ VList []

listWrapImpl :: [Value] -> Scoper Value
listWrapImpl [value] = pure $ VList [value]

consImpl :: [Value] -> Scoper Value
consImpl [cHead, VList cTail] = pure $ VList (cHead:cTail)

listAppendImpl :: [Value] -> Scoper Value
listAppendImpl [first@(VList xs), second@(VList ys)] = do
  assert (typesEqual first second) "Lists must be of the same type"
  pure $ VList (xs <> ys)
listAppendImpl _ = stackTrace "You must append a list to a list"

listMemptyImpl :: [Value] -> Scoper Value
listMemptyImpl [] = pure $ VList []

consStrImpl :: [Value] -> Scoper Value
consStrImpl [VChar x, VList xs] =
  pure $ VList $ [VChar '"', VChar x] <> xs <> [VChar '"']
consStrImpl [x, VList xs] = do
  impl  <- findImplsInScope "str" x
  inner <- sequence $ evaluateImpl impl . pure <$> x:xs
  pure $ VList $ [VChar '['] <>
    intercalate [VChar ',', VChar ' '] (lElements <$> inner) <>
    [VChar ']']

nilStrImpl :: [Value] -> Scoper Value
nilStrImpl [_] = pure $ VList [VChar '[', VChar ']']

listDefinitions :: [(Id, Id, [FunctionCase])]
listDefinitions = [
    ("List", "str", [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"]]
          consStrImpl,
        generateInteropCase
          [PatternArg "Nil" []]
          nilStrImpl
    ]),
    ("List", "map", [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"], IdArg "func"]
          consMapImpl,
        generateInteropCase
          [PatternArg "Nil" [], IdArg "func"]
          nilMapImpl
    ]),
    ("List", "foldl", [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"],
           IdArg "initial",
           IdArg "folder"]
          consFoldlImpl,
        generateInteropCase
          [PatternArg "Nil" [],
           IdArg "initial",
           IdArg "folder"]
          nilFoldlImpl
    ]),
    ("List", "foldr", [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"],
           IdArg "initial",
           IdArg "folder"]
          consFoldrImpl,
        generateInteropCase
          [PatternArg "Nil" [],
           IdArg "initial",
           IdArg "folder"]
          nilFoldrImpl
    ]),
    ("List", "bind", [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"],
           IdArg "func"]
          consBindImpl,
        generateInteropCase
          [PatternArg "Nil" [],
           IdArg "func"]
          nilBindImpl
    ]),
    ("List", "wrap", [
        generateInteropCase
          [IdArg "val"]
          listWrapImpl
    ]),
    ("List", "append", [
        generateInteropCase
          [TypedIdArg "first" "List", TypedIdArg "second" "List"]
          listAppendImpl
    ]),
    ("List", "mempty", [
        generateInteropCase
          []
          listMemptyImpl
    ]),
    ("List", "Nil", [generateInteropCase [] (const . pure $ VList [])]),
    ("List", "Cons", [generateInteropCase [IdArg "head", IdArg "tail"] consImpl])
  ]
