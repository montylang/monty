module CallableUtils (runFun, evaluateCases) where

import Prelude
import Data.List
import Data.Maybe
import Lens.Micro.Platform
import Debug.Trace

import RunnerUtils
import RunnerTypes
import ParserTypes

evaluateTf :: DefSignature -> [FunctionCase] -> [Value] -> Scoper Value
evaluateTf (DefSignature tname fname args retSelf) cases params = do
    case findInferredType args params of
      Just cname -> do
        inferredParams <- inferValues cname params
        funRet <- evaluateCases cases inferredParams
        case (funRet, retSelf) of
          (v@(VInferred _ _ _), True) -> applyInferredType cname v
          (v, _)                      -> pure v
      Nothing    -> pure $ VInferred fname tname params
  where
    inferValues :: Id -> [Value] -> Scoper [Value]
    inferValues cname vals =
      sequence $ applyInferredType cname <$> vals
    
    applyInferredType :: Id -> Value -> Scoper Value
    applyInferredType cname (VInferred ifname _ iparams) = do
      impls <- implForClass cname ifname
      evaluateCases impls iparams
    applyInferredType _ value = pure value

    findInferredType :: [Arg] -> [Value] -> Maybe Id
    findInferredType cargs values = listToMaybe $
      (maybeToList . classForValue . (view _2)) =<<
        (filter ((== SelfArg) . (view _1)) $ zip cargs values)

evaluateCases :: [FunctionCase] -> [Value] -> Scoper Value
evaluateCases cases params = runWithTempScope $ do
  fcase <- pickFun cases params
  _     <- addArgsToScope (fcaseArgs fcase) params
  runFcase fcase

runFcase :: FunctionCase -> Scoper Value
runFcase (FunctionCase _ body) = do
    sequence_ $ evalP <$> beginning
    s            <- use scope
    evaledReturn <- evalP returnExpr
    pure $ case evaledReturn of
      (VFunction _) -> VScoped evaledReturn s
      _             -> evaledReturn
  where
    (beginning, returnExpr) = splitReturn body
runFcase (InteropCase _ body) = body

pickFun :: [FunctionCase] -> [Value] -> Scoper FunctionCase
pickFun cases params = do
  case find (funCaseMatchesParams params) cases of
    Just funCase -> pure funCase 
    Nothing      -> stackTrace $ "No function defined for " <> show params

funCaseMatchesParams :: [Value] -> FunctionCase -> Bool
funCaseMatchesParams params fcase =
  all (uncurry argMatchesParam) $ zip (fcaseArgs fcase) params

argMatchesParam :: Arg -> Value -> Bool
argMatchesParam (IdArg _) _ = True
argMatchesParam (TypedIdArg _ t) (VTypeInstance cname _ _) = t == cname
argMatchesParam (TypedIdArg _ "Int") (VInt _) = True
argMatchesParam (TypedIdArg _ "String") (VString _) = True
argMatchesParam (TypedIdArg _ "List") (VList _) = True
argMatchesParam (PatternArg "Nil" _) (VList []) = True
argMatchesParam (PatternArg "Cons" _) (VList (_:_)) = True
argMatchesParam (PatternArg pname pargs) (VTypeInstance _ tname tvals) =
  pname == tname && (all (uncurry argMatchesParam) (zip pargs tvals))
argMatchesParam _ _ = False

splitReturn :: [PExpr] -> ([PExpr], PExpr)
splitReturn exprs =
  let (beginning, ([Pos _ (ExprReturn returnExpr)])) = splitAt ((length exprs) - 1) exprs
  in (beginning, returnExpr)

addArgsToScope :: [Arg] -> [Value] -> Scoper ()
addArgsToScope fargs values = do
  assert (length fargs == length values) "Mismatched argument length"
  sequence_ $ addArg <$> zip fargs values
  pure ()

addArg :: (Arg, Value) -> Scoper ()

addArg (IdArg name, v) = addToScope name v

addArg (TypedIdArg name _, v) = addToScope name v

addArg (PatternArg "Cons" [h, t], VList (x:xs)) = do
  _ <- addArg (h, x)
  _ <- addArg (t, VList xs)
  pure ()

addArg (PatternArg "Nil" [], _) = pure ()

addArg (PatternArg pname pargs, VTypeInstance _ tname tvals) = do
  assert (pname == tname) $
    "Mismatched pattern match: " <> pname <> "," <> tname

  assert (length pargs == length tvals) $
    "Mismatched argument length for pattern match of " <> pname

  sequence_ $ addArg <$> (zip pargs tvals)
  pure ()

addArg _ = stackTrace "Bad call to pattern matched function"

runFun :: Value -> [Value] -> Scoper Value
runFun (VScoped func fscope) params =
  runWithScope fscope $ runScopedFun func params
runFun (VTypeCons className consName cargs) params = do
  assert ((length cargs) == (length params)) ("Bad type cons call to " <> consName)
  pure $ VTypeInstance className consName params
runFun expr params = runScopedFun expr params

runScopedFun :: Value -> [Value] -> Scoper Value
runScopedFun (VFunction cases) params = evaluateCases cases params
runScopedFun (VTypeFunction defSig tcases) params =
    if elem SelfArg (getDefSigArgs defSig) then
      evaluateTf defSig cases params
    else
      pure $ VInferred (getDefSigFunName defSig) (getDefSigTypeName defSig) params
  where
    cases = (view _2) <$> tcases

runScopedFun _ _ = stackTrace "Error: Bad function call"
