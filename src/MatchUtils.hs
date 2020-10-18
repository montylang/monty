module MatchUtils (argMatchesValue, scopeMatchedValue) where

import RunnerUtils
import RunnerTypes
import ParserTypes

argMatchesValue :: Arg -> Value -> Bool
argMatchesValue (IdArg _) _ = True
argMatchesValue (TypedIdArg _ t) (VTypeInstance cname _ _) = t == cname
argMatchesValue (TypedIdArg _ "Int") (VInt _) = True
argMatchesValue (TypedIdArg _ "Char") (VChar _) = True
argMatchesValue (TypedIdArg _ "List") (VList _) = True
argMatchesValue (PatternArg "Nil" _) (VList []) = True
argMatchesValue (PatternArg "Cons" _) (VList (_:_)) = True
argMatchesValue (PatternArg "Tuple" xs) (VTuple ys) = (length xs) == (length ys)
argMatchesValue (PatternArg pname pargs) (VTypeInstance _ tname tvals) =
  pname == tname && (all (uncurry argMatchesValue) (zip pargs tvals))
argMatchesValue _ _ = False

scopeMatchedValue :: (Arg, Value) -> Scoper ()

-- TODO: Check if the var is already in scope
scopeMatchedValue (IdArg name, v) = addToScope name v

scopeMatchedValue (TypedIdArg name _, v) = addToScope name v

scopeMatchedValue (PatternArg "Cons" [h, t], VList (x:xs)) = do
  _ <- scopeMatchedValue (h, x)
  _ <- scopeMatchedValue (t, VList xs)
  pure ()

scopeMatchedValue (PatternArg "Nil" [], _) = pure ()

scopeMatchedValue (PatternArg "Tuple" args, VTuple values) = do
  assert (length args == length values) $
    "Mismatched argument length for pattern match of tuple"
  
  sequence_ $ scopeMatchedValue <$> (zip args values)
  pure ()

scopeMatchedValue (PatternArg pname pargs, VTypeInstance _ tname tvals) = do
  assert (pname == tname) $
    "Mismatched pattern match: " <> pname <> "," <> tname

  assert (length pargs == length tvals) $
    "Mismatched argument length for pattern match of " <> pname

  sequence_ $ scopeMatchedValue <$> (zip pargs tvals)
  pure ()

scopeMatchedValue _ = stackTrace "Bad call to pattern matched function"
