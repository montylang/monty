module InfixUtils where

import Debug.Trace

import ParserTypes
import RunnerTypes
import TypeUtils

intInfixEval :: Value -> InfixOp -> Value -> Value
intInfixEval (VInt first) InfixAdd (VInt second) = VInt $ first + second
intInfixEval (VInt first) InfixSub (VInt second) = VInt $ first - second
intInfixEval (VInt first) InfixMul (VInt second) = VInt $ first * second
intInfixEval (VInt first) InfixEq (VInt second) = toBoolValue $ first == second
intInfixEval (VInt first) InfixGt (VInt second) = toBoolValue $ first > second
intInfixEval (VInt first) InfixLt (VInt second) = toBoolValue $ first < second
intInfixEval (VInt first) InfixGe (VInt second) = toBoolValue $ first >= second
intInfixEval (VInt first) InfixLe (VInt second) = toBoolValue $ first <= second
intInfixEval _ other _ = trace ("Unimplemented infix: " <> show other) undefined

opToComp :: InfixOp -> Value -> Bool
opToComp InfixGt value = isVInstanceNamed "GT" value
opToComp InfixLt value = isVInstanceNamed "LT" value
opToComp InfixGe value = isVInstanceNamed "GT" value || isVInstanceNamed "EQ" value
opToComp InfixLe value = isVInstanceNamed "LT" value || isVInstanceNamed "EQ" value
opToComp _ _ = undefined

applyUnaryNot :: Value -> Scoper EValue
applyUnaryNot (VTypeInstance "Bool" "True" [])  = pure $ Right vfalse
applyUnaryNot (VTypeInstance "Bool" "False" []) = pure $ Right vtrue
applyUnaryNot value = pure $ Left $ "Cannot negate " <> show value
