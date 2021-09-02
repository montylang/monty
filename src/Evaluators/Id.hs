module Evaluators.Id where

import Evaluators.Evaluatable
import Text.Megaparsec hiding (Pos)

import ParserTypes
import RunnerUtils

data RId = RId
  { rIdPos :: SourcePos,
    rIdValue :: Id
  }

instance Evaluatable RId where
  render RId {rIdValue} = rIdValue
