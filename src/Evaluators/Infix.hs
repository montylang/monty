module Evaluators.Infix where

import Text.Megaparsec hiding (Pos)
import Data.Tuple

import Evaluators.Evaluatable
import ParserTypes
import RunnerTypes
import RunnerUtils
import TypeUtils
import CallableUtils
import PrettyPrint

data RInfix = RInfix
    { rInfixPos :: SourcePos,
      rInfixLhs :: ET,
      rInfixOp  :: InfixOp,
      rInfixRhs :: ET
    }

instance Evaluatable RInfix where
  render (RInfix _ lhs op rhs) =
    render lhs <> " " <> prettyPrint op <> " " <> render rhs
