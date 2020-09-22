{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module InteropPrelude (preludeDefinitions) where

import Control.Monad.State.Strict
import Data.Either

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

consFoldlImpl :: [Value] -> Scoper Value
consFoldlImpl [x, (VList xs), initial, folder] = do
    res <- foldM nativeFolder (Right initial) (x:xs)
    case res of
      Left(err)  -> stackTrace err
      Right(suc) -> pure suc
  where
    nativeFolder :: (Either String Value) -> Value -> Scoper (Either String Value)
    nativeFolder err@(Left _) _ = pure err
    nativeFolder (Right acc) it = runFun folder [acc, it]

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
    unList :: Value -> Either String [Value]
    unList (VList vals) = Right vals
    unList _            = Left "Result of bind on list must be a list"

    joinValues :: [Value] -> Scoper Value
    joinValues values =
      case sequence $ unList <$> values of
        Left err   -> stackTrace err
        Right vals -> pure $ VList $ join vals

nilBindImpl :: [Value] -> Scoper Value
nilBindImpl [_] = pure $ VList []

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
    ("foldl", [
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
    ("len", [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"]]
          consLenImpl,
        generateInteropCase
          [PatternArg "Nil" []]
          nilLenImpl
    ]),
    ("bind", [
        generateInteropCase
          [PatternArg "Cons" [IdArg "head", IdArg "tail"],
           IdArg "func"]
          consBindImpl,
        generateInteropCase
          [PatternArg "Nil" [],
           IdArg "func"]
          nilBindImpl
    ]),
    ("Nil", [generateInteropCase [] (const . pure $ VList [])]),
    ("Cons", [generateInteropCase [IdArg "head", IdArg "tail"] consImpl])
  ]
