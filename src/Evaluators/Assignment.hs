module Evaluators.Assignment (evalAssignment) where

import Data.IORef
import Control.Monad.State.Strict

import ParserTypes
import RunnerTypes
import RunnerUtils
import MatchUtils

evalAssignment :: Arg -> PExpr -> Scoper Value
evalAssignment (IdArg name) value = do
    evaledValue  <- evalP value
    inScopeValue <- findInTopScope name
    
    case inScopeValue of
      (Just (VScoped (VFunction cases) s)) -> do
        newFun <- appendFunctionCase cases evaledValue s
        replaceInScope name newFun
      (Just _) -> stackTrace $ "Cannot mutate " <> name
      _        -> addToScope name evaledValue
    
    pure evaledValue
  where
    appendFunctionCase :: FunctionImpl -> Value -> Scope -> Scoper Value
    appendFunctionCase impl (VScoped (VFunction newImpl) _) newScope = do
      combined <- combineImpls impl newImpl
      pure $ VScoped (VFunction combined) newScope
    appendFunctionCase _ _ _ = stackTrace $ "Cannot mutate " <> name

evalAssignment arg value = do
    evaled  <- evalP value

    case zipArgToValue arg evaled of
      Right res -> (sequence $ uncurry addToScope <$> res) *> pure evaled
      Left err  -> stackTrace err

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
