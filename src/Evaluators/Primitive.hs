module Evaluators.Primitive where

import Text.Megaparsec hiding (Pos)
import Evaluators.Evaluatable

import RunnerTypes
import RunnerUtils

data RInt = RInt
    { rIntPos :: SourcePos,
      rIntVal :: Int
    }

data RDouble = RDouble
    { rDoublePos :: SourcePos,
      rDoubleVal :: Double
    }

data RChar = RChar
    { rCharPos :: SourcePos,
      rCharVal :: Char
    }

data RTuple =
  RTuple
    { rTuplePos :: SourcePos,
      rTupleElements :: [ET]
    }

data RList =
  RList
    { rListPos :: SourcePos,
      rListElements :: [ET]
    }

instance Evaluatable RInt where
  render RInt {rIntVal} = show rIntVal

instance Evaluatable RDouble where
  render RDouble {rDoubleVal} = show rDoubleVal

instance Evaluatable RChar where
  render RChar {rCharVal} = show rCharVal

instance Evaluatable RTuple where
  render RTuple {} = show "<tuple>"

instance Evaluatable RList where
  render RList {} = show "<lit>"
