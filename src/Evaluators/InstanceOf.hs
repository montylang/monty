module Evaluators.InstanceOf where

import Text.Megaparsec hiding (Pos)
import qualified Data.HashMap.Strict as HM
import Data.List

import Evaluators.Evaluatable
import Evaluators.Assignment
import Evaluators.Def
import PrettyPrint
import RunnerTypes
import ParserTypes
import RunnerUtils
import Debug.Trace

data RInstanceOf = RInstanceOf
  { rInstancePos :: SourcePos,
    rInstanceClassName :: Id,
    rInstanceTypeName :: Id,
    rInstanceBody :: [RAssignment RDef]
  }

instance Evaluatable RInstanceOf where
  getPos RInstanceOf {rInstancePos} = rInstancePos
  evaluate RInstanceOf {rInstanceClassName, rInstanceTypeName, rInstanceBody} =
    evalInstanceOf rInstanceClassName rInstanceTypeName rInstanceBody

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
      pure voidValue

addImplementation :: Id -> [Id] -> [DefSignature] -> RAssignment RDef -> Scoper ()
addImplementation cname classTypeCons available
                  (RAssignment {rAssArg = (IdArg name),
                                rAssValue = RDef {rDefArgs, rDefBody}}) = do
  sigArgs  <- getSigArgs name available
  caseArgs <- markArgs cname classTypeCons rDefArgs sigArgs 
  addBodyToScope cname name (runBody rDefBody) caseArgs

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
addBodyToScope cname name body caseArgs = do
  maybeStub <- findInScope name
  case maybeStub of
    Just val -> (replaceInScope name =<< updateStub cname val caseArgs body)
    _        -> stackTrace $ name <> " is not in scope"

updateStub :: Id -> Value -> [Arg] -> (Scoper Value) -> Scoper Value
updateStub cname stub caseArgs body =
  addToStub cname (FunctionCase caseArgs $ body) stub
