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
addArgsToScope fargs values | length fargs /= length values =
  stackTrace "Mismatched argument length"
addArgsToScope fargs values = do
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

addArg (PatternArg pname _, VTypeInstance _ tname _) | (pname /= tname) =
  stackTrace $ "Mismatched pattern match: " <> pname <> "," <> tname

addArg (PatternArg pname pargs, VTypeInstance _ _ tvals)
  | (length pargs /= length tvals) = 
  stackTrace $ "Mismatched argument length for pattern match of " <> pname

addArg (PatternArg _ pargs, VTypeInstance _ _ tvals) = do
  _ <- sequence $ addArg <$> (zip pargs tvals)
  pure ()

addArg _ = stackTrace "Bad call to pattern matched function"
