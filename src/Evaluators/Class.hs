module Evaluators.Class where

import Evaluators.Evaluatable
import Text.Megaparsec hiding (Pos)

import PrettyPrint
import RunnerTypes
import RunnerUtils
import ParserTypes

data RClass = RClass
  { rClassPos :: SourcePos,
    rClassName :: Id,
    rClassTypeCons :: [Pos TypeCons]
  }

instance Evaluatable RClass where
  render RClass {rClassName} = "<class " <> rClassName <> ">"
