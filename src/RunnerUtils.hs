module RunnerUtils where

import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Monad.Except
import Lens.Micro.Platform

import RunnerTypes
import ParserTypes

stackTrace :: String -> Scoper a
stackTrace message = throwError $ ErrString message

assert :: Bool -> String -> Scoper ()
assert False message = throwError $ ErrString message
assert True _        = pure ()

eval :: Expr -> Scoper Value
eval expr = use (executors . evaluateExpr) >>= ($ expr)

evalP :: PExpr -> Scoper Value
evalP pexpr = use (executors . evaluatePExpr) >>= ($ pexpr)

typesEqual :: Value -> Value -> Bool
typesEqual (VTypeInstance t1 _ _) (VTypeInstance t2 _ _) = t1 == t2
typesEqual (VInt _) (VInt _)           = True
typesEqual (VChar _) (VChar _)         = True
typesEqual (VList (x:_)) (VList (y:_)) = typesEqual x y
typesEqual (VList _) (VList _)         = True
typesEqual _ _                         = False

addToScope :: String -> Value -> Scoper ()
addToScope "_" _     = pure ()
addToScope key value = scope %= addToScope'
  where
    addToScope' (topScope:lowerScopes) = newTop:lowerScopes
      where newTop = HM.insert key value topScope
    addToScope' [] = undefined

unionTopScope :: [(Id, Value)] -> Scoper ()
unionTopScope updates = scope %= unionTopScope' (HM.fromList updates)
  where
    unionTopScope' :: ScopeBlock -> Scope -> Scope
    unionTopScope' new (topScopeBlock:bottomBlocks) =
      (HM.union new topScopeBlock):bottomBlocks
    unionTopScope' _ [] = undefined

-- Returns the value for the given key, and the scope block where it is defined
findInScope :: String -> Scoper (Maybe (Value, Scope))
findInScope key = findInScope' <$> use scope
  where
    findInScope' :: Scope -> Maybe (Value, Scope)
    findInScope' [] = Nothing
    findInScope' (top:lower) =
      case HM.lookup key top of
        Just value -> Just (value, top:lower)
        Nothing    -> findInScope' lower

findInTopScope :: String -> Scoper (Maybe Value)
findInTopScope key = findInScope' <$> use scope
  where
    findInScope' (top:_) = HM.lookup key top
    findInScope' _         = Nothing

implForClass :: Id -> Id -> Scoper [FunctionCase]
implForClass cname fname = do
    fnameImplsMaybe <- findInScope fname

    pure $ fromMaybe [] $ do
      fnameImpls <- view _1 <$> fnameImplsMaybe
      cases      <- toCases fnameImpls
      pure $ findImpls cname cases
  where
    toCases :: Value -> Maybe [(Id, FunctionCase)]
    toCases (VTypeFunction _ cases) = Just cases
    toCases _                       = Nothing

    findImpls :: Id -> [(Id, FunctionCase)] -> [FunctionCase]
    findImpls monadClass cases =
      (view _2) <$> filter ((monadClass ==) . (view _1)) cases

findImplsInScope :: Id -> Value -> Scoper [FunctionCase]
findImplsInScope fname value =
  case classForValue value of
    Just cname -> implForClass cname fname
    Nothing    -> pure []

classForValue :: Value -> Maybe Id
classForValue (VList _)   = Just "List"
classForValue (VInt _)    = Just "Int"
classForValue (VChar _)   = Just "Char"
classForValue (VTypeInstance cname _ _) = Just cname
classForValue _ = Nothing

pushScopeBlock :: ScopeBlock -> Scoper ()
pushScopeBlock block = scope %= (block:)

pushEmptyScopeBlock :: Scoper ()
pushEmptyScopeBlock = pushScopeBlock HM.empty

popScopeBlock :: Scoper ()
popScopeBlock = scope %= drop 1

runScopeWithSetup :: Scoper () -> Scoper Value -> Scoper Value
runScopeWithSetup scopeSetup body = do
  previousScope <- get
  scopeSetup
  retVal <- body
  put previousScope
  pure retVal

runWithScope :: Scope -> Scoper Value -> Scoper Value
runWithScope s body = runScopeWithSetup (scope %= const s) body

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

addToStub :: Id -> FunctionCase -> Value -> Scoper Value
addToStub cname newCase (VTypeFunction defSig cases) =
  pure $ VTypeFunction defSig (cases ++ [(cname, newCase)])
addToStub _ _ _ = stackTrace "Cannot add stub case to non v-type function"
