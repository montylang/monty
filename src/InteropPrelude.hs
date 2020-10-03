{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module InteropPrelude (preludeDefinitions) where

import Control.Monad.State.Strict

import ParserTypes
import RunnerTypes
import RunnerUtils
import CallableUtils

debugImpl :: [Value] -> Scoper Value
debugImpl [input] = do
  liftIO $ putStrLn $ show input
  pure input

consMapImpl :: [Value] -> Scoper Value
consMapImpl [x, (VList xs), func] = do
  ranHead <- runFun func [x]
  ranTail <- sequence $ (\el -> runFun func [el]) <$> xs
  pure $ VList (ranHead:ranTail)

nilMapImpl :: [Value] -> Scoper Value
nilMapImpl _ = pure $ VList []

consFoldlImpl :: [Value] -> Scoper Value
consFoldlImpl [x, (VList xs), initial, folder] =
    foldM nativeFolder initial (x:xs)
  where
    nativeFolder :: Value -> Value -> Scoper Value
    nativeFolder acc it = runFun folder [acc, it]

nilFoldlImpl :: [Value] -> Scoper Value
nilFoldlImpl [initial, _] = pure initial

consLenImpl :: [Value] -> Scoper Value
consLenImpl [x, (VList xs)] = pure $ VInt $ length (x:xs)

nilLenImpl :: [Value] -> Scoper Value
nilLenImpl [] = pure $ VInt 0

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
consImpl [cHead, (VList cTail)] = pure $ VList (cHead:cTail)

listAppendImpl :: [Value] -> Scoper Value
listAppendImpl [first@(VList xs), second@(VList ys)] = do
  assert (typesEqual first second) "Lists must be of the same type"
  pure $ VList (xs <> ys)
listAppendImpl _ = stackTrace "You must append a list to a list"

stringAppendImpl :: [Value] -> Scoper Value
stringAppendImpl [(VString xs), (VString ys)] = pure $ VString (xs <> ys)
stringAppendImpl _ = stackTrace "You may only append strings with other strings"

intCompareImpl :: [Value] -> Scoper Value
intCompareImpl [(VInt first), (VInt second)] =
    pure $ ordToVal $ compare first second
  where 
    ordToVal :: Ordering -> Value
    ordToVal a = VTypeInstance "Ordering" (show a) []

intStrImpl :: [Value] -> Scoper Value
intStrImpl [(VInt value)] = pure $ VString $ show value

listDefinitions :: [(Id, Id, [FunctionCase])]
listDefinitions = [
    ("List", "debug", [generateInteropCase [IdArg "value"] debugImpl]),
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
    ("List", "len", [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"]]
          consLenImpl,
        generateInteropCase
          [PatternArg "Nil" []]
          nilLenImpl
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
          [TypedIdArg "val" "List"]
          listWrapImpl
    ]),
    ("List", "append", [
        generateInteropCase
          [TypedIdArg "first" "List", TypedIdArg "second" "List"]
          listAppendImpl
    ]),
    ("List", "Nil", [generateInteropCase [] (const . pure $ VList [])]),
    ("List", "Cons", [generateInteropCase [IdArg "head", IdArg "tail"] consImpl])
  ]

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
    ])
  ]

stringDefinitions :: [(Id, Id, [FunctionCase])]
stringDefinitions = [
    ("String", "append", [
        generateInteropCase
          [TypedIdArg "first" "String", TypedIdArg "second" "String"]
          stringAppendImpl
    ])
  ]

preludeDefinitions :: [(Id, Id, [FunctionCase])]
preludeDefinitions =
  listDefinitions <> intDefinitions <> stringDefinitions
