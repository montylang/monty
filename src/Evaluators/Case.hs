module Evaluators.Case (evalCase) where

import Control.Monad
import Data.List
import Data.Either

import RunnerTypes
import ParserTypes
import RunnerUtils
import MatchUtils

evalCase :: PExpr -> [Pos CaseBlock] -> Scoper Value
evalCase input posBodies = runWithTempScope $ do
    evaledInput <- evalP input
    _           <- cbsType bodies

    case find isRight (prepare evaledInput <$> bodies) of
      Just (Right (body, zipped)) -> do
        sequence_ $ uncurry addToScope <$> zipped
        runBody body
      Nothing -> stackTrace "Non-exhaustive pattern matches for function"
  where
    bodies = getPosValue <$> posBodies

runBody :: [PExpr] -> Scoper Value
runBody exprs = do
    sequence_ $ evalP <$> beginning
    evalP lastExpr
  where
    (beginning, [lastExpr]) = splitAt ((length exprs) - 1) exprs

prepare :: Value -> CaseBlock -> Either String ([PExpr], [(Id, Value)])
prepare input (CaseBlock arg body) = do
  zipped <- zipArgToValue arg input
  pure $ (body, zipped)

cbsType :: [CaseBlock] -> Scoper Type
cbsType blocks = do
  (x:xs) <- sequence $ cbType <$> blocks
  foldM combineType x xs

cbType :: CaseBlock -> Scoper Type
cbType (CaseBlock arg _) = argToType arg
