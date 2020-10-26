module Evaluators.Condition (evalCondition) where

import ParserTypes
import RunnerTypes
import RunnerUtils

evalCondition :: (CondBlock RExpr) -> [CondBlock RExpr] -> [RExpr] -> Scoper Value
evalCondition ifCond elifConds elseBody = do
     selectedBody <- pickBody (ifCond:elifConds)
     evalBody selectedBody
  where
    pickBody :: [CondBlock RExpr] -> Scoper [RExpr]
    pickBody [] = pure $ elseBody
    pickBody ((CondBlock condition condBody):xs) = do
      condVal <- eval condition

      case condVal of
        (VTypeInstance _ "True" _)  -> pure condBody
        (VTypeInstance _ "False" _) -> pickBody xs
        _                           -> stackTrace "Condition is not a boolean"

evalBody :: [RExpr] -> Scoper Value
evalBody exprs = do
  vals <- sequence $ eval <$> exprs
  pure $ last vals
