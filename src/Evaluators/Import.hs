module Evaluators.Import where

import Text.Megaparsec hiding (Pos)
import Data.List
import Control.Lens

import ModuleLoader
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
  evaluate RImport {riPath} = voidValue <$ loadModule riPath

instance PrettyPrint RImport where
  prettyPrint _ = "<import>"
