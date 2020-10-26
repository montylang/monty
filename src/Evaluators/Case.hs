module Evaluators.Case (evalCase) where

import Control.Monad
import Data.List
import Data.Either

import RunnerTypes
import ParserTypes
import RunnerUtils
import MatchUtils

evalCase :: RExpr -> [CaseBlock RExpr] -> Scoper Value
evalCase input bodies = runWithTempScope $ do
  evaledInput <- eval input

  assertCbsType bodies

  case find isRight (prepare evaledInput <$> bodies) of
    Just (Right (body, zipped)) -> do
      sequence_ $ uncurry addToScope <$> zipped
      runBody body
    Nothing -> stackTrace "Non-exhaustive pattern matches for function"

runBody :: [RExpr] -> Scoper Value
runBody exprs = do
    sequence_ $ eval <$> beginning
    eval lastExpr
  where
    (beginning, [lastExpr]) = splitAt ((length exprs) - 1) exprs

prepare :: Value -> CaseBlock RExpr -> Either String ([RExpr], [(Id, Value)])
prepare input (CaseBlock _ arg body) = do
  zipped <- zipArgToValue arg input
  pure $ (body, zipped)

assertCbsType :: [CaseBlock RExpr] -> Scoper ()
assertCbsType blocks = do
  (x:xs) <- sequence $ cbType <$> blocks
  _      <- foldM combineType x xs
  pure ()

cbType :: CaseBlock RExpr -> Scoper Type
cbType (CaseBlock _ arg _) = argToType arg
