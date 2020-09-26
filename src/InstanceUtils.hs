module InstanceUtils where

import Data.List
import Lens.Micro.Platform

import ParserTypes
import RunnerTypes
import RunnerUtils

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
