module Evaluators.Prefix (evalPrefix) where

import ParserTypes
import RunnerTypes
import RunnerUtils
import TypeUtils
import CallableUtils
import PrettyPrint

evalPrefix :: PrefixOp -> RExpr -> Scoper Value
evalPrefix PrefixNegate ex = do
  evaled <- eval ex

  case evaled of
    VInt value -> pure $ VInt (-value)
    VDouble value -> pure $ VDouble (-value)
    _          -> stackTrace "Can only negate numbers"
evalPrefix PrefixNot ex = do
  evaled <- eval ex

  case evaled of
    VTypeInstance "Bool" _ _ ->
      toBoolValue . not <$> valueToBool evaled
    _ -> stackTrace "Can only call not on booleans"
