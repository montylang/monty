module Evaluators.Case where

import Text.Megaparsec hiding (Pos)
import Control.Monad
import Data.List
import Data.Either

import Evaluators.Evaluatable
import RunnerTypes
import ParserTypes
import RunnerUtils
import MatchUtils
import PrettyPrint

data RCase = RCase
  { rCasePos :: SourcePos,
    rCaseInput :: ET,
    rCaseBlocks :: [CaseBlock ET]
  }

instance Evaluatable RCase where
  render _ = "<case>"
