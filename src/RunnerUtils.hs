module RunnerUtils where

import Debug.Trace
import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import System.Exit
import RunnerTypes
import ParserTypes

runtimeError :: String -> Scoper a
runtimeError message = do
  _ <- lift $ die message
  -- Will never get reached, but hey, it fixes compiler errors
  undefined

typeOfValue :: Value -> String
typeOfValue (VInt _)                = "Int"
typeOfValue (VString _)             = "String"
typeOfValue (VBoolean _)            = "Bool"
typeOfValue (VFunction _)           = "Function" -- TODO: Become a signature
typeOfValue (VTypeCons _ _)         = "TypeCons"
typeOfValue (VTypeDef _ _)          = "TypeDef"
typeOfValue (VTypeFunction _ _ _ _) = "Function" -- TODO: Become a signature
typeOfValue (VScoped val _)         = typeOfValue val
typeOfValue (VClass)                = "Class"
typeOfValue (VList [])              = "List()"
typeOfValue (VList (x:_))           = "List(" <> typeOfValue x <> ")"
typeOfValue (VDict)                 = "Dict"
typeOfValue (VTuple)                = "Tuple"
typeOfValue (VTypeInstance typeName values) =
  typeName <> "(" <> intercalate "," (typeOfValue <$> values) <> ")"

argToId :: Arg -> Id
argToId (IdArg name) = name
argToId _ = trace "you bad boi, you" undefined

-- TODO: Don't allow overriding of values in top scope
addToScope :: String -> Value -> Scope -> Scope
addToScope key value (topScope:lowerScopes) = newTop:lowerScopes
  where
    newTop = HM.insert key value topScope
addToScope _ _ [] = undefined

unionTopScope :: ScopeBlock -> Scope -> Scope
-- TODO: Error on name collisions
unionTopScope new (topScopeBlock:bottomBlocks) =
  (HM.union new topScopeBlock):bottomBlocks
unionTopScope _ [] = undefined

-- Returns the value for the given key, and the scope block where it is defined
findInScope :: String -> Scope -> Maybe (Value, Scope)
findInScope _ [] = Nothing
findInScope key (top:lower) =
  case HM.lookup key top of
    Nothing    -> findInScope key lower
    Just value -> Just (value, top:lower)

pushScopeBlock :: ScopeBlock -> Scope -> Scope
pushScopeBlock block scope = block:scope

popScopeBlock :: Scope -> Scope
popScopeBlock [] = []
popScopeBlock (_:bottom) = bottom

scoperAssert :: Bool -> String -> Scoper ()
scoperAssert False message = runtimeError message
scoperAssert True _ = pure ()
