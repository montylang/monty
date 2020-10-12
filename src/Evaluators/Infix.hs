module Evaluators.Infix (evalInfix) where

import Data.Tuple

import ParserTypes
import RunnerTypes
import RunnerUtils
import TypeUtils
import CallableUtils

evalInfix :: PExpr -> InfixOp -> PExpr -> Scoper Value
evalInfix first op second = do
  f' <- evalP first
  s' <- evalP second
  (f, s) <- inferTypes f' s'
  assert (typesEqual f s) "Cannot operate on values of different types"

  case f of
    (VInt _)                   -> intInfixEval f op s
    (VList _)                  -> concatInfixEval f op s
    (VTypeInstance "Bool" _ _) ->
      boolInfixEval (valueToBool f) op (valueToBool s)
    _                          -> genericInfixEval f op s

  where
    inferTypes :: Value -> Value -> Scoper (Value, Value)
    inferTypes v@(VInferred _ _ _) other = inferFromOther v other
    inferTypes other v@(VInferred _ _ _) = swap <$> inferFromOther v other
    inferTypes v1 v2 = pure (v1, v2)

    inferFromOther :: Value -> Value -> Scoper (Value, Value)
    inferFromOther vinf val = 
      case classForValue val of
        Just cname -> do
          inferred <- applyInferredType cname vinf
          pure (inferred, val)
        Nothing    -> stackTrace "Types cannot be inferred from context"

boolInfixEval :: Bool -> InfixOp -> Bool -> Scoper Value
boolInfixEval f InfixLogicAnd s = pure $ toBoolValue (f && s)
boolInfixEval f InfixLogicOr s  = pure $ toBoolValue (f || s)
boolInfixEval f other s = genericInfixEval (toBoolValue f) other (toBoolValue s)

concatInfixEval :: Value -> InfixOp -> Value -> Scoper Value
concatInfixEval first@(VList _) InfixAdd second@(VList _) =
  applyBinaryFun "append" first second
concatInfixEval x op y = genericInfixEval x op y

genericInfixEval :: Value -> InfixOp -> Value -> Scoper Value
genericInfixEval first InfixEq second =
  applyBinaryFun "equals" first second
genericInfixEval first InfixNe second =
  applyUnaryNot =<< applyBinaryFun "equals" first second
genericInfixEval first InfixGt second = compareOrderable first InfixGt second
genericInfixEval first InfixGe second = compareOrderable first InfixGe second
genericInfixEval first InfixLt second = compareOrderable first InfixLt second
genericInfixEval first InfixLe second = compareOrderable first InfixLe second
genericInfixEval first InfixMappend second = applyBinaryFun "append" first second
genericInfixEval _ op _ = stackTrace ("Unimplemented generic infix " <> show op)

compareOrderable :: Value -> InfixOp -> Value -> Scoper Value
compareOrderable f op s =
  (toBoolValue . opToComp op) <$> applyBinaryFun "compare" f s

applyBinaryFun :: Id -> Value -> Value -> Scoper Value
applyBinaryFun fname f s = do
  impl <- findImplsInScope fname f
  evaluateImpl impl [f, s]

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
