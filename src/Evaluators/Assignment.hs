module Evaluators.Assignment (evalAssignment) where

import ParserTypes
import RunnerTypes
import RunnerUtils

evalAssignment :: Id -> PExpr -> Scoper Value
evalAssignment name value = do
    evaledValue  <- evalP value
    inScopeValue <- findInTopScope name
    
    newValue <- case inScopeValue of
      (Just (VFunction cases)) -> appendFunctionCase cases evaledValue
      (Just _)                 -> stackTrace $ "Cannot mutate " <> name
      _                        -> pure evaledValue
    
    addToScope name newValue
    pure evaledValue
  where
    appendFunctionCase :: [FunctionCase] -> Value -> Scoper Value
    appendFunctionCase cases (VFunction [newCase]) = do
      assert (all (functionCaseFits newCase) cases)
        $ "Invalid pattern match for function " <> name
      pure $ VFunction (cases <> [newCase])
    appendFunctionCase _ _ = stackTrace $ "Cannot mutate " <> name

functionCaseFits :: FunctionCase -> FunctionCase -> Bool
functionCaseFits newCase existingCase =
    (length newCaseArgs == length existingCaseArgs) &&
      (any (uncurry argFits) (zip newCaseArgs existingCaseArgs))
  where
    newCaseArgs = fcaseArgs newCase
    existingCaseArgs = fcaseArgs existingCase

argFits :: Arg -> Arg -> Bool
argFits (IdArg _) (PatternArg _ _) = True
argFits (PatternArg newName newArgs) (PatternArg existingName existingArgs) =
  newName /= existingName || all (uncurry argFits) (zip newArgs existingArgs)
argFits _ _ = False
