module Evaluators.Unwrap (evalUnwrap) where

import Data.Maybe

import RunnerTypes
import ParserTypes
import RunnerUtils
import CallableUtils

evalUnwrap :: [PExpr] -> Scoper Value
evalUnwrap = evalUnwrap' Nothing

evalUnwrap' :: Maybe Id -> [PExpr] -> Scoper Value
evalUnwrap' (Just className) [Pos _ (ExprWrap result)] = do
  evaledResult <- evalP result
  impls <- findImplsInScope "wrap" evaledResult

  case impls of
    []     -> stackTrace $ "No wrap implementation for " <> className
    fcases -> evaluateCases fcases [evaledResult]

evalUnwrap' _ [_] =
  stackTrace "Last statement in unwrap must be wrap"

evalUnwrap' (Just className) ((Pos _ (ExprBind var expr)):xs) = do
  evaled <- evalP expr
  inferred <- cuuuuuum evaled
  -- Infer evaled type from className, if it's a vinferred

  if (Just className) == reeee inferred
    then unwrapWithClassname className var inferred xs
    else stackTrace $
      "All binds in unwrap must be of same monad type. " <>
      "Expected '" <> className <> "' got '" <>
      (fromMaybe "<primitive>" (reeee inferred)) <>
      "'"

  where
    cuuuuuum :: Value -> Scoper Value
    cuuuuuum (VInferred _ _ _) = undefined -- INfer stuff here
    cuuuuuum v                 = pure v

    reeee :: Value -> Maybe Id
    reeee (VInferred _ _ _) = Just className
    reeee v                 = classForValue v

evalUnwrap' Nothing ((Pos _ (ExprBind var expr)):xs) = do
  evaledExpr <- evalP expr

  case evaledExpr of
    (VList _)                     -> unwrapWithClassname "List" var evaledExpr xs
    (VTypeInstance className _ _) -> unwrapWithClassname className var evaledExpr xs
    _ -> stackTrace "Initial expr in unwrap is not monadic"

evalUnwrap' _ _ = stackTrace "something terrible has happened"

unwrapWithClassname :: Id -> Id -> Value -> [PExpr] -> Scoper Value
unwrapWithClassname className var expr xs = do
  lambda <- pure $ generateInteropCase [IdArg var] $
    const $ evalUnwrap' (Just className) xs

  impls <- findImplsInScope "bind" expr

  case impls of
    []     -> stackTrace $ "No bind implementation for " <> className
    fcases -> evaluateCases fcases [expr, VFunction [lambda]]
