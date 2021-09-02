module Evaluators.Condition where

import Text.Megaparsec hiding (Pos)
import Data.List

import Evaluators.Evaluatable
import ParserTypes
import RunnerTypes
import RunnerUtils

data RCondition = RCondition
  { rConditionPos :: SourcePos,
    rConditionIf :: CondBlock ET,
    rConditionElifs :: [CondBlock ET],
    rConditionElseBody :: [ET]
  }

instance Evaluatable RCondition where
  render RCondition {rConditionIf, rConditionElifs, rConditionElseBody} =
    "<if statement>"
