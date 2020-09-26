module Evaluators.Condition (evalCondition) where

import ParserTypes
import RunnerTypes
import RunnerUtils

evalCondition :: CondBlock -> [CondBlock] -> [PExpr] -> Scoper Value
evalCondition ifCond elifConds elseBody = do
     selectedBody <- pickBody (ifCond:elifConds)
     evalBody selectedBody
  where
    pickBody :: [CondBlock] -> Scoper [PExpr]
    pickBody [] = pure $ elseBody
    pickBody ((CondBlock condition condBody):xs) = do
      condVal <- evalP condition

      case condVal of
        (VTypeInstance _ "True" _)  -> pure condBody
        (VTypeInstance _ "False" _) -> pickBody xs
        _                           -> stackTrace "Condition is not a boolean"

evalBody :: [PExpr] -> Scoper Value
evalBody exprs = do
  vals <- sequence $ evalP <$> exprs
  pure $ last vals
