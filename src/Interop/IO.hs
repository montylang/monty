module Interop.IO (ioDefinitions) where

import Control.Monad.State.Strict
import Data.Foldable (foldrM)
import Data.Char (ord, chr)
import Control.Exception (catch)

import ParserTypes
import PrettyPrint
import TypeUtils
import RunnerTypes
import RunnerUtils
import CallableUtils

import Interop.Helpers

ioPrintStrT :: [Value] -> Scoper Value
ioPrintStrT [VList str@((VChar _):_), token] = do
  liftIO $ putStrLn $ (vChr <$> str)
  pure $ VTuple [unitValue, token]

ioReadStrT :: [Value] -> Scoper Value
ioReadStrT [token] = do
    input <- maybeStringToValue <$> liftIO safeGetLine
    pure $ VTuple [input, token]
  where
    eToMaybe :: IOError -> IO (Maybe String)
    eToMaybe _ = pure Nothing

    safeGetLine :: IO (Maybe String)
    safeGetLine = catch (Just <$> getLine) eToMaybe

    maybeStringToValue :: Maybe String -> Value
    maybeStringToValue (Just s) =
      VTypeInstance "Maybe" "Just" [VList $ VChar <$> s]
    maybeStringToValue Nothing  =
      VTypeInstance "Maybe" "None" []

ioDefinitions :: [(Id, Id, [FunctionCase])]
ioDefinitions = [
    ("Any", "printStrT", [
        generateInteropCase
          [TypedIdArg "input" "List",
           TypedIdArg "world" "#IOWorldToken"]
          ioPrintStrT
    ]),
    ("Any", "readStrT", [
        generateInteropCase
          [TypedIdArg "world" "#IOWorldToken"]
          ioReadStrT
    ])
  ]
