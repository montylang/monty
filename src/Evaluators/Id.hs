module Evaluators.Id where

import Evaluators.Evaluatable
import Text.Megaparsec hiding (Pos)

import ParserTypes
import RunnerUtils
import PrettyPrint

data RId = RId
  { rIdPos :: SourcePos,
    rIdValue :: Id
  }

instance Evaluatable RId where
  getPos RId {rIdPos} = rIdPos
  evaluate (RId _ "_") = stackTrace "Cannot use black hole as variable"
  evaluate (RId _ name) = do
    value <- findInScope name
    case value of
      Just val -> pure val
      Nothing -> stackTrace (name <> " is not in scope")

instance PrettyPrint RId where
  prettyPrint RId {rIdValue} = rIdValue
