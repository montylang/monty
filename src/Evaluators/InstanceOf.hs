module Evaluators.InstanceOf where

import Text.Megaparsec hiding (Pos)
import qualified Data.HashMap.Strict as HM
import Data.List
import Control.Lens

import Evaluators.Evaluatable
import Evaluators.Assignment
import Evaluators.Def
import PrettyPrint
import RunnerTypes
import ParserTypes
import RunnerUtils
import Debug.Trace

data RInstanceOf = RInstanceOf
  { _rInstancePos :: SourcePos,
    _rInstanceClassName :: Id,
    _rInstanceTypeName :: Id,
    _rInstanceImpls :: [RAssignment RDef]
  }

$(makeLenses ''RInstanceOf)

instance Evaluatable RInstanceOf where
  render _ = "<instanceof>"
