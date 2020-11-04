module Evaluators.Case where

import Text.Megaparsec hiding (Pos)
import Control.Monad
import Data.List
import Data.Either

import Evaluators.Evaluatable
import RunnerTypes
import ParserTypes
import RunnerUtils
import MatchUtils
import PrettyPrint

data RCase = RCase
  { rCasePos :: SourcePos,
    rCaseInput :: ET,
    rCaseBlocks :: [CaseBlock ET]
  }

instance Evaluatable RCase where
  getPos RCase {rCasePos} = rCasePos
  evaluate RCase {rCaseInput, rCaseBlocks} = evalCase rCaseInput rCaseBlocks

instance PrettyPrint RCase where
  prettyPrint RCase {rCaseInput, rCaseBlocks} =
    "<case>"
    -- "case " <> prettyPrint rCaseInput <> ":\n" <>
    -- (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> rCaseBlocks) <> "\n"

evalCase :: ET -> [CaseBlock ET] -> Scoper Value
evalCase input bodies = runWithTempScope $ do
  evaledInput <- evaluate input

  assertCbsType bodies

  case find isRight (prepare evaledInput <$> bodies) of
    Just (Right (body, zipped)) -> do
      sequence_ $ uncurry addToScope <$> zipped
      runCaseBody body
    Nothing -> stackTrace "Non-exhaustive pattern matches for function"

runCaseBody :: [ET] -> Scoper Value
runCaseBody exprs = do
    sequence_ $ evaluate <$> beginning
    evaluate lastExpr
  where
    (beginning, [lastExpr]) = splitAt ((length exprs) - 1) exprs

prepare :: Value -> CaseBlock ET -> Either String ([ET], [(Id, Value)])
prepare input (CaseBlock _ arg body) = do
  zipped <- zipArgToValue arg input
  pure $ (body, zipped)

assertCbsType :: [CaseBlock ET] -> Scoper ()
assertCbsType blocks = do
  (x:xs) <- sequence $ cbType <$> blocks
  _      <- foldM combineType x xs
  pure ()

cbType :: CaseBlock ET -> Scoper Type
cbType (CaseBlock _ arg _) = argToType arg
