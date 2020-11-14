module Evaluators.InstanceOf where

import Text.Megaparsec hiding (Pos)
import qualified Data.HashMap.Strict as HM
import Data.List
import Control.Lens

import Evaluators.Evaluatable
import Evaluators.Assignment
import Evaluators.Def
import PrettyPrint
import RunnerTypes
import ParserTypes
import RunnerUtils
import Debug.Trace

data RInstanceOf = RInstanceOf
  { _rInstancePos :: SourcePos,
    _rInstanceClassName :: Id,
    _rInstanceTypeName :: Id,
    _rInstanceImpls :: [RAssignment RDef]
  }

$(makeLenses ''RInstanceOf)

instance Evaluatable RInstanceOf where
  getPos instanceOf = instanceOf ^. rInstancePos
  evaluate (RInstanceOf _ className typeName implementations) = do
      classDef <- findInTypeScope className
      typeDef  <- findInTypeScope typeName
      funcDefs <- typeDefSigs typeDef
      
      if className == "List" then
        addAllImplementations ["Nil", "Cons"] funcDefs
      else case classDef of
        Just (VClass consNames) -> addAllImplementations consNames funcDefs
        _ -> stackTrace $ "Attempted to use undefined class: " <> className
    where
      typeDefSigs :: Maybe Value -> Scoper [DefSignature]
      typeDefSigs (Just (VTypeDef _ sigs)) = pure sigs
      typeDefSigs _ = stackTrace $ "Type " <> typeName <> " not found"
  
      addAllImplementations :: [Id] -> [DefSignature] -> Scoper Value
      addAllImplementations consNames defSigs = do
        sequence_ $ (addImplementation className consNames defSigs)
          <$> implementations
        pure unitValue
    
evalInstanceOf :: Id -> Id -> [RAssignment RDef] -> Scoper Value
evalInstanceOf className typeName implementations = do
    classDef <- findInTypeScope className
    typeDef  <- findInTypeScope typeName
    funcDefs <- typeDefSigs typeDef
    
    if className == "List" then
      addAllImplementations ["Nil", "Cons"] funcDefs
    else case classDef of
      Just (VClass consNames) -> addAllImplementations consNames funcDefs
      _ -> stackTrace $ "Attempted to use undefined class: " <> className
  where
    typeDefSigs :: Maybe Value -> Scoper [DefSignature]
    typeDefSigs (Just (VTypeDef _ sigs)) = pure sigs
    typeDefSigs _ = stackTrace $ "Type " <> typeName <> " not found"

    addAllImplementations :: [Id] -> [DefSignature] -> Scoper Value
    addAllImplementations consNames defSigs = do
      sequence_ $ (addImplementation className consNames defSigs)
        <$> implementations
      pure unitValue

addImplementation :: Id -> [Id] -> [DefSignature] -> RAssignment RDef -> Scoper ()
addImplementation cname classTypeCons available ass = do
  -- TODO: Will die if not IdArg... Probably shouldn't
  let name = ass ^. rAssArg . idArgVal 
  let def = ass ^. rAssValue
  sigArgs  <- getSigArgs name available
  caseArgs <- markArgs cname classTypeCons (def ^. rDefArgs) sigArgs 
  addBodyToScope cname name (runBody $ def ^. rDefBody) caseArgs

getSigArgs :: Id -> [DefSignature] -> Scoper [Arg]
getSigArgs cname cavailable =
  case find ((cname ==) . getDefSigFunName) cavailable of
    Just sig -> pure $ getDefSigArgs sig
    Nothing  -> stackTrace $
      cname <> " is not part of type " <> (getDefSigTypeName $ head cavailable)

markArgs :: Id -> [Id] -> [Arg] -> [Arg] -> Scoper [Arg]
markArgs cname classes argsA dargs =
  sequence $ (uncurry (validateArgs cname classes)) <$> zip dargs argsA

validateArgs :: Id -> [Id] -> Arg -> Arg -> Scoper Arg
validateArgs cname _ SelfArg (IdArg argName) = pure $ TypedIdArg argName cname
validateArgs cname classes SelfArg (PatternArg pname _) | not $ elem pname classes =
  stackTrace $ "Type constructor " <> pname <> " is not an " <> cname
validateArgs _ _ _ arg = pure arg

addBodyToScope :: Id -> Id -> Scoper Value -> [Arg] -> Scoper ()
addBodyToScope cname fname body caseArgs = do
  maybeStub <- findInScope fname
  case maybeStub of
    Just val -> (replaceInScope fname =<< updateStub fname cname val caseArgs body)
    _        -> stackTrace $ fname <> " is not in scope"

updateStub :: Id -> Id -> Value -> [Arg] -> (Scoper Value) -> Scoper Value
updateStub fname cname stub caseArgs body =
  addToStub fname cname (FunctionCase caseArgs $ body) stub
