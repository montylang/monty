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
  getPos RBlock {rBlockPos} = rBlockPos
  evaluate rcall@(RBlock {rBlockBody}) = runWithTempScope $ runBody rBlockBody

instance PrettyPrint RBlock where
  prettyPrint _ = "<Shhhh this isn't a real thing>"
