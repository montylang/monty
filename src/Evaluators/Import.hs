module Evaluators.Import where

import Text.Megaparsec hiding (Pos)
import Data.List
import Control.Lens

import ParserTypes
import RunnerTypes
import Evaluators.Evaluatable
import PrettyPrint
import RunnerUtils

data RImport = RImport
  { rpos :: SourcePos,
    riPath :: [String]
  }

instance Evaluatable RImport where
  getPos _ = undefined
  evaluate RImport {riPath} = unitValue <$ loadModule riPath

instance PrettyPrint RImport where
  prettyPrint _ = "<import>"
