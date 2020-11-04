module Evaluators.Condition where

import Text.Megaparsec hiding (Pos)
import Data.List

import Evaluators.Evaluatable
import ParserTypes
import RunnerTypes
import RunnerUtils
import PrettyPrint

data RCondition = RCondition
  { rConditionPos :: SourcePos,
    rConditionIf :: CondBlock ET,
    rConditionElifs :: [CondBlock ET],
    rConditionElseBody :: [ET]
  }

instance PrettyPrint RCondition where
  prettyPrint RCondition {rConditionIf, rConditionElifs, rConditionElseBody} =
    "<if statement>"
    -- "if " <> prettyPrint rConditionIf <>
    -- (intercalate "" $ (\x -> "elif " <> prettyPrint x) <$> rConditionElifs) <>
    -- "else:\n" <>
    -- (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> rConditionElseBody)

instance Evaluatable RCondition where
  getPos RCondition {rConditionPos} = rConditionPos
  evaluate RCondition {rConditionIf, rConditionElifs, rConditionElseBody} =
    evalCondition rConditionIf rConditionElifs rConditionElseBody

evalCondition :: (CondBlock ET)
  -> [CondBlock ET]
  -> [ET]
  -> Scoper Value
evalCondition ifCond elifConds elseBody = do
     selectedBody <- pickBody (ifCond:elifConds)
     evalBody selectedBody
  where
    pickBody :: [CondBlock ET] -> Scoper [ET]
    pickBody [] = pure $ elseBody
    pickBody ((CondBlock condition condBody):xs) = do
      condVal <- evaluate condition

      case condVal of
        (VTypeInstance _ "True" _)  -> pure condBody
        (VTypeInstance _ "False" _) -> pickBody xs
        _                           -> stackTrace "Condition is not a boolean"

evalBody :: [ET] -> Scoper Value
evalBody body = do
  vals <- sequence $ evaluate <$> body
  pure $ last vals
