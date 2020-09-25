module CallableUtils where

import Prelude
import Data.List

import RunnerUtils
import RunnerTypes
import ParserTypes

pickFun :: [FunctionCase] -> [Value] -> Scoper FunctionCase
pickFun cases params = do
  case find (funCaseMatchesParams params) cases of
    Just funCase -> pure funCase 
    Nothing      -> stackTrace $ "No function defined for " <> show params

funCaseMatchesParams :: [Value] -> FunctionCase -> Bool
funCaseMatchesParams params fcase =
  all (uncurry argMatchesParam) $ zip (fcaseArgs fcase) params

argMatchesParam :: Arg -> Value -> Bool
argMatchesParam (IdArg _) _ = True
argMatchesParam (TypedIdArg _ t) (VTypeInstance cname _ _) = t == cname
argMatchesParam (TypedIdArg _ "Int") (VInt _) = True
argMatchesParam (PatternArg "Nil" _) (VList []) = True
argMatchesParam (PatternArg "Cons" _) (VList (_:_)) = True
argMatchesParam (PatternArg pname pargs) (VTypeInstance _ tname tvals) =
  pname == tname && (all (uncurry argMatchesParam) (zip pargs tvals))
argMatchesParam _ _ = False

splitReturn :: [PExpr] -> ([PExpr], PExpr)
splitReturn exprs =
  let (beginning, ([Pos _ (ExprReturn returnExpr)])) = splitAt ((length exprs) - 1) exprs
  in (beginning, returnExpr)

addArgsToScope :: [Arg] -> [Value] -> Scoper ()
addArgsToScope fargs values = do
  assert (length fargs == length values) "Mismatched argument length"
  _ <- sequence $ addArg <$> zip fargs values
  pure ()

addArg :: (Arg, Value) -> Scoper ()

addArg (IdArg name, v) = addToScope name v

addArg (TypedIdArg name _, v) = addToScope name v

addArg (PatternArg "Cons" [h, t], VList (x:xs)) = do
  _ <- addArg (h, x)
  _ <- addArg (t, VList xs)
  pure ()

addArg (PatternArg "Nil" [], _) = pure ()

addArg (PatternArg pname pargs, VTypeInstance _ tname tvals) = do
  assert (pname == tname) $
    "Mismatched pattern match: " <> pname <> "," <> tname

  assert (length pargs == length tvals) $
    "Mismatched argument length for pattern match of " <> pname

  _ <- sequence $ addArg <$> (zip pargs tvals)
  pure ()

addArg _ = stackTrace "Bad call to pattern matched function"
