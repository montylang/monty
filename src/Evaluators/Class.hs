module Evaluators.Class where

import Evaluators.Evaluatable
import Text.Megaparsec hiding (Pos)

import PrettyPrint
import RunnerTypes
import ParserTypes
import RunnerUtils

data RClass = RClass
  { rClassPos :: SourcePos,
    rClassName :: Id,
    rClassTypeCons :: [Pos TypeCons]
  }

instance Evaluatable RClass where
  getPos RClass {rClassPos} = rClassPos
  evaluate RClass {rClassName, rClassTypeCons} = do
      addToTypeScope rClassName (VClass consNames)
      unionTopScope $ convert <$> getPosValue <$> rClassTypeCons
      pure unitValue
    where
      convert :: TypeCons -> (Id, Value)
      convert (TypeCons name args) = (name, VTypeCons rClassName name args)

      consNames = getTypeConsName . getPosValue <$> rClassTypeCons

instance PrettyPrint RClass where
  prettyPrint RClass {rClassName} = "<class " <> rClassName <> ">"
