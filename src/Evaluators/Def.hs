module Evaluators.Def where

import Text.Megaparsec hiding (Pos)
import Data.List
import Control.Lens

import ParserTypes
import RunnerTypes
import PrettyPrint
import Evaluators.Evaluatable
import RunnerUtils
import Debug.Trace

data RDef = RDef
  { _rDefPos :: SourcePos,
    _rDefName :: Maybe Id,
    _rDefArgs :: [Arg],
    _rDefBody :: [ET]
  }

makeLenses ''RDef

instance Evaluatable RDef where
  render (RDef _ name args body) =
    "def" <> "(" <> intercalate "," (prettyPrint <$> args) <> "):\n  " <>
      intercalate "\n  " (render <$> body)
