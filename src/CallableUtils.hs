module CallableUtils where

import Prelude
import Data.List (find)
import Debug.Trace (trace)

import RunnerUtils
import RunnerTypes
import ParserTypes

pickFun :: [FunctionCase] -> [Value] -> Scoper FunctionCase
pickFun cases params = do
  case find (funCaseMatchesParams params) cases of
    Just funCase -> pure funCase 
    -- FIXME: Better error message
    Nothing      -> runtimeError $ "No function defined for " <> show params

funCaseMatchesParams :: [Value] -> FunctionCase -> Bool
funCaseMatchesParams params (FunctionCase fargs _) =
  all argMatchesParams $ zip fargs params

argMatchesParams :: (Arg, Value) -> Bool
argMatchesParams ((IdArg _), _) = True
argMatchesParams ((PatternArg pname _), (VTypeInstance tname _)) =
  pname == tname
argMatchesParams _ = False

splitReturn :: [Expr] -> ([Expr], Expr)
splitReturn exprs =
  let (beginning, [ExprReturn returnExpr]) = splitAt ((length exprs) - 1) exprs in
    (beginning, returnExpr)

addArgsToScope :: [Arg] -> [Value] -> Scoper ()
addArgsToScope fargs values = do
  scoperAssert (length fargs == length values) "Mismatched argument length"
  _ <- sequence $ addArg <$> (zip fargs values)
  pure ()

addArg :: (Arg, Value) -> Scoper ()
addArg ((IdArg name), v) = trace name $ addToScope name v
addArg ((PatternArg pname pargs), (VTypeInstance tname tvals)) = do
  scoperAssert (pname == tname)
    $ "Mismatched pattern match: " <> pname <> "," <> tname
  scoperAssert (length pargs == length tvals)
    $ "Mismatched argument length for pattern match of " <> pname
  -- TODO: Recursively pattern match
  _ <- sequence $ addArg <$> (zip pargs tvals)
  pure ()
addArg _ =
  runtimeError $ "Bad call to pattern matched function"
