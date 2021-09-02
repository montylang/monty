module Evaluators.Call where

import Text.Megaparsec hiding (Pos)
import Data.List
import Control.Lens

import ParserTypes
import RunnerTypes
import Evaluators.Evaluatable
import PrettyPrint
import RunnerUtils
import CallableUtils

data RCall = RCall
  { rCallPos :: SourcePos,
    rCallFun :: ET,
    rCallParams :: [ET]
  }

instance Evaluatable RCall where
  render RCall {rCallFun, rCallParams} =
    render rCallFun <> "(" <> intercalate "," (render <$> rCallParams) <> ")"
