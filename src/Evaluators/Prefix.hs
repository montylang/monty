module Evaluators.Prefix where

import Text.Megaparsec hiding (Pos)

import Evaluators.Evaluatable
import ParserTypes
import RunnerTypes
import RunnerUtils
import TypeUtils
import CallableUtils
import PrettyPrint

data RPrefix = RPrefix
    { rPrefixPos :: SourcePos,
      rPrefixOp :: PrefixOp,
      rPrefixRhs :: ET
    }

instance Evaluatable RPrefix where
  getPos RPrefix {rPrefixPos} = rPrefixPos
  evaluate (RPrefix _ op rhs) = do
    rhsValue <- eval rhs
    evalPrefix op rhsValue

instance PrettyPrint RPrefix where
  prettyPrint (RPrefix _ op rhs) =
    "<prefix>"
    --"(" <> prettyPrint op <> prettyPrint rhs <> ")"

evalPrefix :: PrefixOp -> Value -> Scoper Value
evalPrefix PrefixNegate rhs = do
  case rhs of
    VInt value -> pure $ VInt (-value)
    VDouble value -> pure $ VDouble (-value)
    _          -> stackTrace "Can only negate numbers"
evalPrefix PrefixNot rhs = do
  case rhs of
    VTypeInstance "Bool" _ _ ->
      toBoolValue . not <$> valueToBool rhs
    _ -> stackTrace "Can only call not on booleans"
