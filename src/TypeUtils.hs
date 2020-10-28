module TypeUtils where

import Debug.Trace

import RunnerUtils
import RunnerTypes
import ParserTypes

vtrue :: Value
vtrue  = VTypeInstance "Bool" "True" []

vfalse :: Value
vfalse = VTypeInstance "Bool" "False" []

toBoolValue :: Bool -> Value
toBoolValue True  = vtrue
toBoolValue False = vfalse

valueToBool :: Value -> Scoper Bool
valueToBool (VTypeInstance "Bool" "True" _)  = pure True
valueToBool (VTypeInstance "Bool" "False" _) = pure False
valueToBool v = stackTrace $ show v <> " is not a bool"

isVInstanceNamed :: Id -> Value -> Bool
isVInstanceNamed expected (VTypeInstance _ iname _) = iname == expected
isVInstanceNamed _ _ = False

isVInstanceOf :: Id -> Value -> Bool
isVInstanceOf expected (VTypeInstance tname _ _) = tname == expected
isVInstanceOf _ _ = False
