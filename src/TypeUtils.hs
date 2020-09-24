module TypeUtils where

import RunnerTypes
import ParserTypes

vtrue :: Value
vtrue  = VTypeInstance "Bool" "True" []

vfalse :: Value
vfalse = VTypeInstance "Bool" "False" []

toBoolValue :: Bool -> Value
toBoolValue True  = vtrue
toBoolValue False = vfalse

isVInstanceNamed :: Id -> Value -> Bool
isVInstanceNamed expected (VTypeInstance _ iname _) = iname == expected
isVInstanceNamed _ _ = False

isVInstanceOf :: Id -> Value -> Bool
isVInstanceOf expected (VTypeInstance tname _ _) = tname == expected
isVInstanceOf _ _ = False
