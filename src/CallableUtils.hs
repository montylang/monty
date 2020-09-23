module CallableUtils where

import Prelude
import Data.List (find)
import Debug.Trace

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
funCaseMatchesParams params fcase =
  all (uncurry argMatchesParam) $ zip (fcaseArgs fcase) params

argMatchesParam :: Arg -> Value -> Bool
argMatchesParam (IdArg _) _ = True
argMatchesParam (TypedIdArg _ t) (VTypeInstance cname _ _) = t == cname
argMatchesParam (PatternArg "Nil" _) (VList []) = True
argMatchesParam (PatternArg "Cons" _) (VList (_:_)) = True
argMatchesParam (PatternArg pname pargs) (VTypeInstance _ tname tvals) =
  pname == tname && (all (uncurry argMatchesParam) (zip pargs tvals))
argMatchesParam _ _ = False

splitReturn :: [PExpr] -> ([PExpr], PExpr)
splitReturn exprs =
  let (beginning, ([Pos _ (ExprReturn returnExpr)])) = splitAt ((length exprs) - 1) exprs
  in (beginning, returnExpr)

addArgsToScope :: [Arg] -> [Value] -> Scoper (Either String ())
addArgsToScope fargs values | length fargs /= length values =
  pure $ Left "Mismatched argument length"
addArgsToScope fargs values = do
  eitherList <- sequence $ addArg <$> zip fargs values
  pure $ const () <$> sequence eitherList

addArg :: (Arg, Value) -> Scoper (Either String ())

addArg (IdArg name, v) = Right <$> addToScope name v

addArg (TypedIdArg name _, v) = Right <$> addToScope name v

addArg (PatternArg "Cons" [h, t], VList (x:xs)) = do
  headE <- addArg (h, x)
  tailE <- addArg (t, VList xs)
  pure $ headE *> tailE

addArg (PatternArg "Nil" [], _) = pure $ Right ()

addArg (PatternArg pname _, VTypeInstance _ tname _) | (pname /= tname) =
  pure $ Left $ "Mismatched pattern match: " <> pname <> "," <> tname

addArg (PatternArg pname pargs, VTypeInstance _ _ tvals)
  | (length pargs /= length tvals) = 
  pure $ Left $ "Mismatched argument length for pattern match of " <> pname

addArg (PatternArg _ pargs, VTypeInstance _ _ tvals) = do
  _ <- sequence $ addArg <$> (zip pargs tvals)
  pure $ Right ()

addArg _ = pure $ Left "Bad call to pattern matched function"
