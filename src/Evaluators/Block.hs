module Evaluators.Block where

import Text.Megaparsec hiding (Pos)
import Data.List
import Control.Lens

import ParserTypes
import RunnerTypes
import Evaluators.Evaluatable
import Evaluators.Def
import PrettyPrint
import RunnerUtils
import CallableUtils

data RBlock = RBlock
  { rBlockPos :: SourcePos,
    rBlockBody :: [ET]
  }

instance Evaluatable RBlock where
  render _ = "<Shhhh this isn't a real thing>"
