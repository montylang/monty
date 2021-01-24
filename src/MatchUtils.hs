module MatchUtils (zipArgToValue, zipArgsToValues) where

import Data.Either
import Control.Monad

import RunnerUtils
import RunnerTypes
import ParserTypes

zipArgsToValues :: [Arg] -> [Value] -> Either String [(Id, Value)]
zipArgsToValues args values =
  join <$> sequence (uncurry zipArgToValue <$> zip args values)

zipArgToValue :: Arg -> Value -> Either String [(Id, Value)]
zipArgToValue (IdArg name) val = Right [(name, val)]
zipArgToValue (TypedIdArg name t) val@(VTypeInstance cname _ _) =
  if t == cname
    then Right [(name, val)]
    else Left $ "Expected " <> name <> ", got " <> cname

zipArgToValue (TypedIdArg name "Int") val@(VInt _)       = Right [(name, val)]
zipArgToValue (TypedIdArg name "Double") val@(VDouble _) = Right [(name, val)]
zipArgToValue (TypedIdArg name "Char") val@(VChar _)     = Right [(name, val)]
zipArgToValue (TypedIdArg name "List") val@(VList _)     = Right [(name, val)]
zipArgToValue (TypedIdArg name "Tuple") val@(VTuple _)   = Right [(name, val)]

zipArgToValue (IntArg expected) (VInt actual) =
  if expected == actual
    then Right []
    else Left $ "Expected " <> show expected <> ", got " <> show actual

zipArgToValue (CharArg expected) (VChar actual) =
  if expected == actual
    then Right []
    else Left $ "Expected " <> show expected <> ", got " <> show actual

zipArgToValue (PatternArg "Nil" _) (VList []) = Right []

zipArgToValue (PatternArg "Cons" [headArg, tailArg]) (VList (x:xs)) = do
  headZipped <- zipArgToValue headArg x
  tailZipped <- zipArgToValue tailArg (VList xs)
  pure $ headZipped <> tailZipped

zipArgToValue (PatternArg "Tuple" xs) (VTuple ys) = zipArgsToValues xs ys

zipArgToValue (PatternArg pname pargs) (VTypeInstance _ tname tvals) =
  if pname == tname && length pargs == length tvals
    then zipArgsToValues pargs tvals
    else Left $ "Mismatched length or pattern match: " <> pname <> "," <> tname

zipArgToValue _ _ = Left "Bad pattern match"
