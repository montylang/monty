module InstanceUtils  where

import Data.List

import ParserTypes
import RunnerTypes
import RunnerUtils

addImplementation :: Id -> [Id] -> [DefSignature] -> Expr -> Scoper (Either String ())
addImplementation cname classTypeCons available
                  (ExprAssignment name (Pos _ (ExprDef args body))) =
  case markArgs cname classTypeCons args =<< (getSigArgs name available) of
    Right caseArgs -> addBodyToScope name body caseArgs
    Left err       -> pure $ Left err
addImplementation _ _ _ _ =
  pure $ Left "Every root expr in an implementation must be a def"

getSigArgs :: Id -> [DefSignature] -> Either String [Arg]
getSigArgs cname cavailable =
  case find ((cname ==) . getDefSigFunName) cavailable of
    Just sig -> Right $ getDefSigArgs sig
    Nothing -> Left $ -- TODO: Types must currently have at least one function
      cname <> " is not part of type " <> (getDefSigTypeName $ head cavailable)

updateStub :: Value -> [Arg] -> [PExpr] -> Value
updateStub stub caseArgs body =
  addToStub (FunctionCase caseArgs $ body) stub

markArgs :: Id -> [Id] -> [Arg] -> [Arg] -> Either String [Arg]
markArgs cname classes argsA dargs =
  sequence $ (uncurry (validateArgs cname classes)) <$> zip dargs argsA

validateArgs :: Id -> [Id] -> Arg -> Arg -> Either String Arg
validateArgs cname _ SelfArg (IdArg argName) = Right $ TypedIdArg argName cname
validateArgs cname classes SelfArg (PatternArg pname _) | not $ elem pname classes =
  Left $ "Type constructor " <> pname <> " is not an " <> cname
validateArgs _ _ _ arg = Right arg

addBodyToScope :: Id -> [PExpr] -> [Arg] -> Scoper (Either String ())
addBodyToScope name body caseArgs = do
  maybeStub <- findInScope name
  case maybeStub of
    Just (ree, _) -> Right <$> (addToScope name $ updateStub ree caseArgs body)
    _             -> pure $ Left $ name <> " is not in scope"
