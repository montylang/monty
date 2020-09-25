module InfixUtils where

import ParserTypes
import RunnerTypes
import TypeUtils
import RunnerUtils

intInfixEval :: Value -> InfixOp -> Value -> Scoper Value
intInfixEval (VInt first) InfixAdd (VInt second) = pure $ VInt $ first + second
intInfixEval (VInt first) InfixSub (VInt second) = pure $ VInt $ first - second
intInfixEval (VInt first) InfixMul (VInt second) = pure $ VInt $ first * second
intInfixEval (VInt first) InfixEq (VInt second) = pure $ toBoolValue $ first == second
intInfixEval (VInt first) InfixGt (VInt second) = pure $ toBoolValue $ first > second
intInfixEval (VInt first) InfixLt (VInt second) = pure $ toBoolValue $ first < second
intInfixEval (VInt first) InfixGe (VInt second) = pure $ toBoolValue $ first >= second
intInfixEval (VInt first) InfixLe (VInt second) = pure $ toBoolValue $ first <= second
intInfixEval _ other _ = stackTrace ("Unimplemented infix: " <> show other)

opToComp :: InfixOp -> Value -> Bool
opToComp InfixGt value = isVInstanceNamed "GT" value
opToComp InfixLt value = isVInstanceNamed "LT" value
opToComp InfixGe value = isVInstanceNamed "GT" value || isVInstanceNamed "EQ" value
opToComp InfixLe value = isVInstanceNamed "LT" value || isVInstanceNamed "EQ" value
opToComp _ _ = undefined

applyUnaryNot :: Value -> Scoper Value
applyUnaryNot (VTypeInstance "Bool" "True" [])  = pure $ vfalse
applyUnaryNot (VTypeInstance "Bool" "False" []) = pure $ vtrue
applyUnaryNot value = stackTrace $ "Cannot negate " <> show value
