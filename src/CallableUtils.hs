module CallableUtils (runFun, evaluateImpl, applyInferredType) where

import Prelude
import Data.List
import Data.Maybe
import Control.Lens
import Debug.Trace
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Monad.State.Strict

import MatchUtils
import RunnerUtils
import RunnerTypes
import ParserTypes
import PrettyPrint
import Data.Either (isRight)

evaluateTf :: DefSignature -> HM.HashMap Id FunctionImpl -> [Value] -> Scoper Value
evaluateTf (DefSignature tname fname args retSelf) impls params = do
    case inferredType of
      Just cname -> do
        inferredParams <- inferValues cname
        impl <- justToScoper
          ("No impl of " <> fname <> " for " <> cname)
          (HM.lookup cname impls)
        funRet <- evaluateImpl impl inferredParams
        case (funRet, retSelf) of
          (v@VInferred {}, True) -> applyInferredType cname v
          (v, _)                      -> pure v
      Nothing    -> pure $ VInferred fname tname params
  where
    justToScoper :: String -> Maybe a -> Scoper a
    justToScoper _ (Just val)    = pure val
    justToScoper message Nothing = stackTrace message
    
    argVals :: [(Arg, Value)]
    argVals = zip args params

    inferValues :: Id -> Scoper [Value]
    inferValues cname =
      sequence $ inferSelfArg cname <$> argVals
    
    inferredType :: Maybe Id
    inferredType = listToMaybe $
      (maybeToList . classForValue . view _2) =<<
        filter ((== SelfArg) . view _1) argVals
    
    inferSelfArg :: Id -> (Arg, Value) -> Scoper Value
    inferSelfArg cname (SelfArg, value) = applyInferredType cname value
    inferSelfArg _ (_, value) = pure value

applyInferredType :: Id -> Value -> Scoper Value
applyInferredType cname (VInferred ifname _ iparams) = do
  impl <- implForClass cname ifname
  evaluateImpl impl iparams
applyInferredType _ value = pure value

tryInferral :: Id -> Id -> [Value] -> Scoper Value
tryInferral target fname vals = do
  impl <- implForClass target fname
  evaluateImpl impl vals

fitValueToType :: Maybe Id -> Type -> Value -> Scoper Value
fitValueToType _ (TUser tid) v@(VTypeInstance tname _ _) | tid == tname = pure v
fitValueToType name t@(TUser tid) v@(VInferred fname _ vals) =
  fitValueToType name t =<< tryInferral tid fname vals
fitValueToType _ TAnything v                  = pure v
fitValueToType _ TInt v@(VInt _)              = pure v
fitValueToType _ TDouble v@(VDouble _)        = pure v
fitValueToType _ TChar v@(VChar _)            = pure v
fitValueToType _ (TUser "List") v@(VList _)   = pure v
fitValueToType _ (TUser "Tuple") v@(VTuple _) = pure v
fitValueToType name t v =
    stackTrace $ nameStr <> " expected: '" <>
      prettyPrint t <> "' but got: '" <> prettyPrint v <> "'"
  where
    nameStr = fromMaybe "Function" name

evaluateImpl :: FunctionImpl -> [Value] -> Scoper Value
evaluateImpl (FunctionImpl name cases typeSig) values = do
  inferred <- sequence $ uncurry (fitValueToType name) <$> zip typeSig values
  evaluateCases cases inferred

evaluateCases :: [FunctionCase] -> [Value] -> Scoper Value
evaluateCases cases params = runWithTempScope $ do
  case find isRight (prepare <$> cases) of
    Just (Right (fcase, zipped)) -> do
      sequence_ $ uncurry addToScope <$> zipped
      fcaseBody fcase
    Nothing -> stackTrace "Non-exhaustive pattern matches for function"
  where
    prepare :: FunctionCase -> Either String (FunctionCase, [(Id, Value)])
    prepare fcase =
      if length (fcaseArgs fcase) == length params then do
        inside <- zipArgsToValues (fcaseArgs fcase) params
        pure (fcase, inside)
      else Left $
        "Mismatched argument length. Got: " <>
        show (prettyPrint <$> params) <> ", but expected: " <>
        show (show <$> fcaseArgs fcase)

runFun :: Value -> [Value] -> Scoper Value
runFun (VScoped func fscope) params = do
  runWithScope fscope $ runScopedFun func params
runFun og@(VTypeCons className consName cargs) params =
  curryWrapper
    og
    (pure $ VTypeInstance className consName params)
    ("Too many args passed to type cons: " <> consName)
    params
runFun value params = runScopedFun value params

handleCurrying :: Int -> [Value] -> Value -> Value
handleCurrying expectedLen params thingToCurry = 
    VFunction $ FunctionImpl Nothing [
        generateInteropCase newArgs
          (\rest -> runFun thingToCurry (newParams <> rest))
      ] newTypes
  where
    actualLen   = length params
    newArgs     = IdArg . (<> "#") . show <$> [1..diff]
    diff        = expectedLen - actualLen
    newParams   = take (diff + 1) params
    newTypes    = drop actualLen $ extractTypeSig thingToCurry

    extractTypeSig :: Value -> [Type]
    extractTypeSig (VFunction (FunctionImpl _ _ types)) = types
    extractTypeSig (VTypeCons _ _ vals) = TAnything <$ vals
    extractTypeSig _ = []

curryWrapper :: Value -> Scoper Value -> String -> [Value] -> Scoper Value
curryWrapper og evaluator errMessage params =
  case compare expectedLen (length params) of
    EQ -> evaluator
    LT -> stackTrace errMessage
    GT -> pure $ handleCurrying expectedLen params og
  where
    expectedLen = argLenForFuncLike og

    argLenForFuncLike :: Value -> Int
    argLenForFuncLike (VTypeCons _ _ cargs) =
      length cargs
    argLenForFuncLike (VTypeFunction (DefSignature _ _ args _) _) =
      length args
    argLenForFuncLike (VFunction (FunctionImpl _ (x:_) _)) =
      length $ fcaseArgs x
    argLenForFuncLike v = trace ("Not a func-like: " <> show v) undefined

runScopedFun :: Value -> [Value] -> Scoper Value
runScopedFun og@(VFunction impl) params =
  curryWrapper
    og
    (evaluateImpl impl params)
    ("Too many args passed to function" <>
     show (prettyPrint <$> params) <> prettyPrint impl)
    params
runScopedFun og@(VTypeFunction defSig tcases) params =
    curryWrapper
      og
      evaluator
      "Too many args passed to function"
      params
  where
    evaluator = if elem SelfArg (getDefSigArgs defSig)
      then evaluateTf defSig tcases params
      else pure $ VInferred
                    (getDefSigFunName defSig)
                    (getDefSigTypeName defSig)
                    params

runScopedFun og _ = stackTrace $ "Error: Bad function call (" <> show og <> ")"
