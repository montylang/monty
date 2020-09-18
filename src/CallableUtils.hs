module CallableUtils where

import Prelude
import Data.List (find)

import RunnerUtils
import RunnerTypes
import ParserTypes

pickFun :: [FunctionCase] -> [Value] -> Scoper FunctionCase
pickFun cases params = do
  case find (funCaseMatchesParams params) cases of
    Just funCase -> pure funCase 
    -- FIXME: Better error message
    Nothing      -> runtimeError $ "No function defined for " <> show (typeOfValue <$> params)

funCaseMatchesParams :: [Value] -> FunctionCase -> Bool
funCaseMatchesParams params fcase =
  all (uncurry argMatchesParam) $ zip (fcaseArgs fcase) params

argMatchesParam :: Arg -> Value -> Bool
argMatchesParam (IdArg _) _ = True
argMatchesParam (PatternArg "Nil" _) (VList []) = True
argMatchesParam (PatternArg "Cons" _) (VList (_:_)) = True
argMatchesParam (PatternArg pname pargs) (VTypeInstance tname tvals) =
  pname == tname && (all (uncurry argMatchesParam) (zip pargs tvals))
argMatchesParam _ _ = False

splitReturn :: [PExpr] -> ([PExpr], PExpr)
splitReturn exprs =
  let (beginning, ([Pos _ (ExprReturn returnExpr)])) = splitAt ((length exprs) - 1) exprs
  in (beginning, returnExpr)

addArgsToScope :: [Arg] -> [Value] -> Scoper ()
addArgsToScope fargs values = do
  scoperAssert (length fargs == length values) "Mismatched argument length"
  _ <- sequence $ addArg <$> (zip fargs values)
  pure ()

addArg :: (Arg, Value) -> Scoper ()
addArg (IdArg name, v) = addToScope name v
addArg (PatternArg "Cons" [h, t], VList (x:xs)) = do
  addArg (h, x)
  addArg (t, VList xs)
addArg (PatternArg "Nil" [], _) = pure ()
addArg (PatternArg pname pargs, VTypeInstance tname tvals) = do
  scoperAssert (pname == tname)
    $ "Mismatched pattern match: " <> pname <> "," <> tname
  scoperAssert (length pargs == length tvals)
    $ "Mismatched argument length for pattern match of " <> pname
  _ <- sequence $ addArg <$> (zip pargs tvals)
  pure ()
addArg _ =
  runtimeError $ "Bad call to pattern matched function"
