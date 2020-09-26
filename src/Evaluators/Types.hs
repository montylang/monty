module Evaluators.Types (evalClass, evalType, evalInstanceOf) where

import Data.List
import Lens.Micro.Platform

import ParserTypes
import RunnerTypes
import RunnerUtils

evalClass :: Id -> [Pos TypeCons] -> Scoper Value
evalClass className constructors = do
    addToScope className (VClass consNames)
    unionTopScope $ convert <$> getPosValue <$> constructors
    pure $ VInt 0 -- TODO: Return... something other than an int :)
  where
    convert :: TypeCons -> (Id, Value)
    convert (TypeCons name args) = (name, VTypeCons className name args)

    consNames = getTypeConsName . getPosValue <$> constructors

evalType :: Id -> [Pos DefSignature] -> Scoper Value
evalType typeName headers = do
    addToScope typeName typeDef
    unionTopScope $ defSigToKeyValue <$> getPosValue <$> headers
    pure $ VInt 0
  where
    typeDef = VTypeDef typeName $ getPosValue <$> headers

    defSigToKeyValue :: DefSignature -> (Id, Value)
    defSigToKeyValue (DefSignature tName functionName args) =
      (functionName,  VTypeFunction tName functionName args [])

-- TODO: Ban redefining instances for classes
evalInstanceOf :: Id -> Id -> [PExpr] -> Scoper Value
evalInstanceOf className typeName implementations = do
    classDef <- findInScope className
    typeDef  <- findInScope typeName
    funcDefs <- functionDefs typeDef
    
    case classDef of
      Just (VClass consNames, _) -> addAllImplementations consNames funcDefs
      _ -> stackTrace $ "Attempted to use undefined class: " <> className
  where
    functionDefs :: Maybe (Value, Scope) -> Scoper [DefSignature]
    functionDefs (Just (VTypeDef _ sigs, _)) = pure sigs
    functionDefs _ = stackTrace $ "Type " <> typeName <> " not found"

    addAllImplementations :: [Id] -> [DefSignature] -> Scoper Value
    addAllImplementations consNames funcDefs = do
      _ <- sequence $ (addImplementation className consNames funcDefs)
                      <$> getPosValue <$> implementations
      pure $ VInt 0 -- TODO: you know what you've done

addImplementation :: Id -> [Id] -> [DefSignature] -> Expr -> Scoper ()
addImplementation cname classTypeCons available
                  (ExprAssignment name (Pos _ (ExprDef args body))) = do
  sigArgs  <- getSigArgs name available
  caseArgs <- markArgs cname classTypeCons args sigArgs 
  addBodyToScope cname name body caseArgs
addImplementation _ _ _ _ =
  stackTrace "Every root expr in an implementation must be a def"

getSigArgs :: Id -> [DefSignature] -> Scoper [Arg]
getSigArgs cname cavailable =
  case find ((cname ==) . getDefSigFunName) cavailable of
    Just sig -> pure $ getDefSigArgs sig
    Nothing  -> stackTrace $ -- TODO: Types must currently have at least one function
      cname <> " is not part of type " <> (getDefSigTypeName $ head cavailable)

updateStub :: Id -> Value -> [Arg] -> [PExpr] -> Scoper Value
updateStub cname stub caseArgs body =
  addToStub cname (FunctionCase caseArgs $ body) stub

markArgs :: Id -> [Id] -> [Arg] -> [Arg] -> Scoper [Arg]
markArgs cname classes argsA dargs =
  sequence $ (uncurry (validateArgs cname classes)) <$> zip dargs argsA

validateArgs :: Id -> [Id] -> Arg -> Arg -> Scoper Arg
validateArgs cname _ SelfArg (IdArg argName) = pure $ TypedIdArg argName cname
validateArgs cname classes SelfArg (PatternArg pname _) | not $ elem pname classes =
  stackTrace $ "Type constructor " <> pname <> " is not an " <> cname
validateArgs _ _ _ arg = pure arg

addBodyToScope :: Id -> Id -> [PExpr] -> [Arg] -> Scoper ()
addBodyToScope cname name body caseArgs = do
  maybeStub <- findInScope name
  case (view _1) <$> maybeStub of
    Just val -> (addToScope name =<< updateStub cname val caseArgs body)
    _        -> stackTrace $ name <> " is not in scope"
