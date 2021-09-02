module Evaluators.Assignment where

import Data.IORef
import Control.Monad.State.Strict
import Text.Megaparsec hiding (Pos)
import Control.Lens

import Evaluators.Evaluatable
import PrettyPrint
import ParserTypes
import RunnerTypes
import RunnerUtils
import MatchUtils

data RAssignment a where
  RAssignment :: Evaluatable a =>
    { _rAssPos :: SourcePos,
      _rAssArg :: Arg,
      _rAssValue :: a
    } -> RAssignment a

rAssArg :: Lens' (RAssignment a) Arg
rAssArg f ass@RAssignment {_rAssArg} =
  (\rAssArg' -> ass {_rAssArg = rAssArg'}) <$> f _rAssArg

rAssValue :: Lens' (RAssignment a) a
rAssValue f ass@RAssignment {_rAssValue} =
  (\rAssValue' -> ass {_rAssValue = rAssValue'}) <$> f _rAssValue

instance Evaluatable (RAssignment a) where
  render (RAssignment _ rAssArg rAssValue) =
    prettyPrint rAssArg <> " = " <> render rAssValue
