module Evaluators.Assignment where

import Data.IORef
import Control.Monad.State.Strict
import Text.Megaparsec hiding (Pos)
import Control.Lens

import Evaluators.Evaluatable
import PrettyPrint
import ParserTypes
import RunnerTypes
import RunnerUtils
import MatchUtils

data RAssignment a where
  RAssignment :: Evaluatable a =>
    { _rAssPos :: SourcePos,
      _rAssArg :: Arg,
      _rAssValue :: a
    } -> RAssignment a

rAssPos :: Lens' (RAssignment a) SourcePos
rAssPos f ass@RAssignment {_rAssPos} =
  (\rAssPos' -> ass {_rAssPos = rAssPos'}) <$> f _rAssPos

rAssArg :: Lens' (RAssignment a) Arg
rAssArg f ass@RAssignment {_rAssArg} =
  (\rAssArg' -> ass {_rAssArg = rAssArg'}) <$> f _rAssArg

rAssValue :: Lens' (RAssignment a) a
rAssValue f ass@RAssignment {_rAssValue} =
  (\rAssValue' -> ass {_rAssValue = rAssValue'}) <$> f _rAssValue

instance Evaluatable (RAssignment a) where
  getPos assignment = assignment ^. rAssPos
  evaluate (RAssignment _ rAssArg rAssValue) = evalAssignment rAssArg rAssValue

instance PrettyPrint a => PrettyPrint (RAssignment a) where
  prettyPrint (RAssignment _ rAssArg rAssValue) =
    prettyPrint rAssArg <> " = " <> prettyPrint rAssValue

evalAssignment :: Evaluatable a => Arg -> a -> Scoper Value
evalAssignment (IdArg name) rhs = do
    evaledValue  <- eval rhs
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

evalAssignment arg rhs = do
    evaled  <- eval rhs

    case zipArgToValue arg evaled of
      Right res -> evaled <$ sequence (uncurry addToScope <$> res)
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
