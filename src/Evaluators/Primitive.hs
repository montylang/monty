module Evaluators.Primitive where

import Text.Megaparsec hiding (Pos)
import Evaluators.Evaluatable

import RunnerTypes
import PrettyPrint
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
  getPos RInt {rIntPos} = rIntPos
  evaluate RInt {rIntVal} = pure $ VInt rIntVal

instance Evaluatable RDouble where
  getPos RDouble {rDoublePos} = rDoublePos
  evaluate RDouble {rDoubleVal} = pure $ VDouble rDoubleVal

instance Evaluatable RChar where
  getPos RChar {rCharPos} = rCharPos
  evaluate RChar {rCharVal} = pure $ VChar rCharVal

instance Evaluatable RTuple where
  getPos RTuple {rTuplePos} = rTuplePos
  evaluate RTuple {rTupleElements} = VTuple <$> (sequence $ eval <$> rTupleElements)

instance Evaluatable RList where
  getPos RList {rListPos} = rListPos
  evaluate RList {rListElements = []} = pure $ VList []
  evaluate RList {rListElements = (x:xs)} = do
      headEvaled <- eval x
      tailEvaled <- sequence $ enforceType headEvaled <$> xs
      pure $ VList (headEvaled:tailEvaled)
    where
      enforceType :: Evaluatable a => Value -> a -> Scoper Value
      enforceType headVal expr = do
        evaled <- eval expr
        assert (typesEqual evaled headVal) "List must be of the same type"
        pure evaled

instance PrettyPrint RInt where
  prettyPrint RInt {rIntVal} = show rIntVal

instance PrettyPrint RDouble where
  prettyPrint RDouble {rDoubleVal} = show rDoubleVal

instance PrettyPrint RChar where
  prettyPrint RChar {rCharVal} = show rCharVal

-- TODO: Propper pp instance
instance PrettyPrint RTuple where
  prettyPrint RTuple {} = show "<tuple>"

-- TODO: Propper pp instance
instance PrettyPrint RList where
  prettyPrint RList {} = show "<lit>"

  
