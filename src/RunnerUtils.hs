module RunnerUtils where

import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens
import Data.IORef

import RunnerTypes
import ParserTypes

import Debug.Trace

stackTrace :: String -> Scoper a
stackTrace message = do
  cp <- use currentPos
  callStack %= (cp:)
  throwError $ ErrString message

assert :: Bool -> String -> Scoper ()
assert False message = stackTrace message
assert True _        = pure ()

unitValue :: Value
unitValue = VTuple []

loadModule :: [String] -> Scoper ()
loadModule path = do
  lm <- use loadModuleImpl
  lm path

typesEqual :: Value -> Value -> Bool
typesEqual (VTypeInstance t1 _ _) (VTypeInstance t2 _ _) = t1 == t2
typesEqual (VInt _) (VInt _)                   = True
typesEqual (VDouble _) (VDouble _)             = True
typesEqual (VChar _) (VChar _)                 = True
typesEqual (VList (x:_)) (VList (y:_))         = typesEqual x y
typesEqual (VList _) (VList _)                 = True
typesEqual VInferred {} VInferred {} = True
typesEqual (VTuple xs) (VTuple ys) =
  length xs == length ys &&
  all (uncurry typesEqual) (zip xs ys)
typesEqual _ _                                 = False

replaceInScope :: String -> Value -> Scoper ()
replaceInScope key value = use scope >>= addToScope' key value

addToScope :: String -> Value -> Scoper ()
addToScope "_" _     = pure ()
addToScope key value = do
  inScopeValue <- findInTopScope key
  assert (isNothing inScopeValue) $ "Cannot reassign " <> key
  use scope >>= addToScope' key value

addToScope' :: String -> Value -> Scope -> Scoper ()
addToScope' key value (topRef:_) =
  liftIO $ modifyIORef topRef $ HM.insert key value
addToScope' _ _ [] = undefined

unionTopScope :: [(Id, Value)] -> Scoper ()
unionTopScope updates = do
  (top:_) <- use scope
  liftIO $ modifyIORef top $ HM.union (HM.fromList updates)

-- Returns the value for the given key, and the scope block where it is defined
findInScope :: String -> Scoper (Maybe Value)
findInScope key = use scope >>= findInScope'
  where
    findInScope' :: Scope -> Scoper (Maybe Value)
    findInScope' [] = pure Nothing
    findInScope' (topRef:botRef) = do
      top <- liftIO $ readIORef topRef
      
      case HM.lookup key top of
        Just value -> pure $ Just value
        Nothing    -> findInScope' botRef

findInTopScope :: String -> Scoper (Maybe Value)
findInTopScope key = do
  (top:_) <- use scope
  topVal <- liftIO $ readIORef top
  pure $ HM.lookup key topVal

implForClass :: Id -> Id -> Scoper FunctionImpl
implForClass cname fname = do
    valueMaybe <- findInScope fname
    case valueMaybe >>= findImpl of
      Just impl -> pure impl
      Nothing   -> stackTrace $
        "No impl of '" <> fname <> "' found for '" <> cname <> "'"
  where
    findImpl :: Value -> Maybe FunctionImpl
    findImpl (VTypeFunction _ impls) = HM.lookup cname impls
    findImpl _                       = Nothing

findImplsInScope :: Id -> Value -> Scoper FunctionImpl
findImplsInScope fname value =
  case classForValue value of
    Just cname -> implForClass cname fname
    Nothing    -> stackTrace $ show value <> " is not a class"

findInTypeScope :: String -> Scoper (Maybe Value)
findInTypeScope key = HM.lookup key <$> use typeScope

addToTypeScope :: String -> Value -> Scoper ()
addToTypeScope key value = do
  tscope <- use typeScope
  assert (isNothing $ HM.lookup key tscope) $ "Cannot reassign " <> key
  typeScope %= HM.insert key value

classForValue :: Value -> Maybe Id
classForValue (VList _)   = Just "List"
classForValue (VTuple _)  = Just "Tuple"
classForValue (VInt _)    = Just "Int"
classForValue (VDouble _) = Just "Double"
classForValue (VChar _)   = Just "Char"
classForValue (VTypeInstance cname _ _) = Just cname
classForValue _ = Nothing

pushScopeBlock :: ScopeBlock -> Scoper ()
pushScopeBlock block = scope %= (block:)

pushEmptyScopeBlock :: Scoper ()
pushEmptyScopeBlock = do
  emptyTop <- liftIO $ newIORef HM.empty
  pushScopeBlock emptyTop

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
runWithScope s = runScopeWithSetup (scope %= const s)

runWithTempScope :: Scoper Value -> Scoper Value
runWithTempScope = runScopeWithSetup pushEmptyScopeBlock

generateInteropCase :: [Arg] -> ([Value] -> Scoper Value) -> FunctionCase
generateInteropCase args fun = FunctionCase args $ do
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

addToStub :: Id -> Id -> FunctionCase -> Value -> Scoper Value
addToStub fname cname newCase (VTypeFunction defSig impls) = do
    res <- case impls ^. at cname of
      Just impl -> updateClassImpl newCase impl
      Nothing -> caseToImpl newCase
    pure $ VTypeFunction defSig $ HM.insert cname res impls
  where
    caseToImpl :: FunctionCase -> Scoper FunctionImpl
    caseToImpl fcase = do
      types <- sequence $ argToType <$> fcaseArgs fcase
      pure $ FunctionImpl (Just fname) [fcase] types

    updateClassImpl :: FunctionCase -> FunctionImpl -> Scoper FunctionImpl
    updateClassImpl newCase oldImpl = do
      newImpl <- caseToImpl newCase
      combineImpls oldImpl newImpl

addToStub _ _ _ _ = stackTrace "Cannot add stub case to non v-type function"

combineImpls :: FunctionImpl -> FunctionImpl -> Scoper FunctionImpl
combineImpls (FunctionImpl xName xCases xTypes) (FunctionImpl _ yCases yTypes) = do
  typeSig <- combineTypes xTypes yTypes
  pure $ FunctionImpl xName (xCases <> yCases) typeSig

combineTypes :: [Type] -> [Type] -> Scoper [Type]
combineTypes xs ys = sequence $ uncurry combineType <$> zip xs ys

combineType :: Type -> Type -> Scoper Type
combineType TAnything new = pure new
combineType old TAnything = pure old
combineType old new = if new == old
  then pure old
  else stackTrace $
    "Cannot have pattern matched function for different types. " <>
    " Got " <> show new <> ", expected " <> show old

argToType :: Arg -> Scoper Type
argToType (IdArg _)               = pure TAnything
argToType (TypedIdArg _ "Char")   = pure TChar
argToType (TypedIdArg _ "Int")    = pure TInt
argToType (TypedIdArg _ "Tuple")  = pure $ TUser "Tuple"
argToType (TypedIdArg _ "Double") = pure TDouble
argToType (TypedIdArg _ t)        = pure $ TUser t
argToType (PatternArg "Cons" _)   = pure $ TUser "List"
argToType (PatternArg "Nil" _)    = pure $ TUser "List"
argToType (PatternArg name _)     = do
  lookup <- findInScope name
  case lookup >>= valueToType of
    Just t -> pure t
    Nothing -> stackTrace $ "Could not find type for " <> name
argToType (IntArg _)  = pure TInt
argToType (CharArg _) = pure TChar

valueToType :: Value -> Maybe Type
valueToType (VTypeCons t _ _) = Just $ TUser t
valueToType (VInt _)          = Just TInt
valueToType (VDouble _)       = Just TDouble
valueToType (VChar _)         = Just TChar
valueToType (VList _)         = Just $ TUser "List"
valueToType (VTuple _)        = Just $ TUser "Tuple"
valueToType _                 = Nothing
