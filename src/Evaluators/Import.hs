module Evaluators.Import where

import Text.Megaparsec hiding (Pos)
import Data.List
import Control.Lens

import ParserTypes
import RunnerTypes
import Evaluators.Evaluatable
import RunnerUtils

data RImport = RImport
  { rpos :: SourcePos,
    riPath :: [String]
  }

instance Evaluatable RImport where
  render _ = "<import>"
