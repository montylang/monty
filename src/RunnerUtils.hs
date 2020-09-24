module RunnerUtils where

import Debug.Trace
import Data.Maybe
import Data.List
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Monad.Except
import Lens.Micro
import Lens.Micro.Extras
import System.Exit

import RunnerTypes
import ParserTypes

runtimeError :: String -> Scoper a
runtimeError message = do
  _ <- liftIO $ die message
  -- Will never get reached, but hey, it fixes compiler errors
  undefined

stackTrace :: String -> Scoper Value
stackTrace message = throwError $ ErrString message

typesEqual :: Value -> Value -> Bool
typesEqual (VTypeInstance t1 _ _) (VTypeInstance t2 _ _) = t1 == t2
typesEqual (VInt _) (VInt _)       = True
typesEqual (VString _) (VString _) = True
typesEqual _ _                     = False

-- TODO: Don't allow overriding of values in top scope
addToScope :: String -> Value -> Scoper ()
addToScope "_" _     = pure ()
addToScope key value = modify addToScope'
  where
    addToScope' (topScope:lowerScopes) = newTop:lowerScopes
      where newTop = HM.insert key value topScope
    addToScope' [] = undefined

unionTopScope :: [(Id, Value)] -> Scoper ()
unionTopScope = modify . unionTopScope' . HM.fromList
  where
    unionTopScope' new (topScopeBlock:bottomBlocks) =
      (HM.union new topScopeBlock):bottomBlocks
    unionTopScope' _ [] = undefined

-- Returns the value for the given key, and the scope block where it is defined
findInScope :: String -> Scoper (Maybe (Value, Scope))
findInScope = gets . findInScope'
  where
    findInScope' _ [] = Nothing
    findInScope' key (top:lower) =
      case HM.lookup key top of
        Nothing    -> findInScope' key lower
        Just value -> Just (value, top:lower)

findInTopScope :: String -> Scoper (Maybe Value)
findInTopScope = gets . findInScope'
  where
    findInScope' key (top:_) = HM.lookup key top
    findInScope' _ _         = Nothing

findImplsInScope :: Id -> Value -> Scoper [FunctionCase]
findImplsInScope fname value = do
    fnameImplsMaybe <- findInScope fname

    pure $ fromMaybe [] $ do
      cname      <- classForValue value
      fnameImpls <- view _1 <$> fnameImplsMaybe
      cases      <- toCases fnameImpls
      pure $ findImpls cname cases
  where
    toCases :: Value -> Maybe [(Id, FunctionCase)]
    toCases (VTypeFunction _ _ _ cases) = Just cases
    toCases _                           = Nothing

    findImpls :: Id -> [(Id, FunctionCase)] -> [FunctionCase]
    findImpls monadClass cases =
      (view _2) <$> filter ((monadClass ==) . (view _1)) cases

classForValue :: Value -> Maybe Id
classForValue (VList _) = Just "List"
classForValue (VInt _) = Just "Int"
classForValue (VTypeInstance cname _ _) = Just cname
classForValue _ = Nothing

pushScopeBlock :: ScopeBlock -> Scoper ()
pushScopeBlock block = do
  scope <- get
  put (block:scope)

pushEmptyScopeBlock :: Scoper ()
pushEmptyScopeBlock = pushScopeBlock HM.empty

popScopeBlock :: Scoper ()
popScopeBlock = do
  scopes <- get
  case scopes of 
    []     -> pure ()
    (_:xs) -> put xs

runScopeWithSetup :: Scoper () -> Scoper Value -> Scoper Value
runScopeWithSetup scopeSetup body = do
  previousScope <- get
  scopeSetup
  retVal <- body
  put previousScope
  pure retVal

runWithScope :: Scope -> Scoper Value -> Scoper Value
runWithScope = runScopeWithSetup . put

runWithTempScope :: Scoper Value -> Scoper Value
runWithTempScope = runScopeWithSetup pushEmptyScopeBlock

generateInteropCase :: [Arg] -> ([Value] -> Scoper Value) -> FunctionCase
generateInteropCase args fun = InteropCase args $ do
    inputValues <- findArgsInScope args
    fun inputValues
  where
    idsInArg :: Arg -> [Id]
    idsInArg (IdArg name) = [name]
    idsInArg (TypedIdArg name _) = [name]
    idsInArg (PatternArg _ cargs) = cargs >>= idsInArg
    
    findArgsInScope :: [Arg] -> Scoper [Value]
    findArgsInScope cargs = do
        values <- sequence (findInTopScope <$> ids)
        pure $ fromJust <$> values
      where 
        ids = cargs >>= idsInArg

addToStub :: Id -> FunctionCase -> Value -> Value
addToStub cname newCase (VTypeFunction tname fname args cases) =
  VTypeFunction tname fname args (cases ++ [(cname, newCase)])
addToStub _ _ _ = trace "Rat pies" undefined
