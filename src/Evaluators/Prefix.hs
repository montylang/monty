module Evaluators.Prefix where

import Text.Megaparsec hiding (Pos)

import Evaluators.Evaluatable
import ParserTypes
import RunnerTypes
import RunnerUtils
import TypeUtils
import CallableUtils

data RPrefix = RPrefix
    { rPrefixPos :: SourcePos,
      rPrefixOp :: PrefixOp,
      rPrefixRhs :: ET
    }

instance Evaluatable RPrefix where
  render (RPrefix _ op rhs) = "<prefix>"
