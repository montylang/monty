module CallableUtils (runFun, evaluateCases, applyInferredType) where

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
    case inferredType of
      Just cname -> do
        inferredParams <- inferValues cname
        funRet <- evaluateCases cases inferredParams
        case (funRet, retSelf) of
          (v@(VInferred _ _ _), True) -> applyInferredType cname v
          (v, _)                      -> pure v
      Nothing    -> pure $ VInferred fname tname params
  where
    argVals :: [(Arg, Value)]
    argVals = zip args params

    inferValues :: Id -> Scoper [Value]
    inferValues cname =
      sequence $ inferSelfArg cname <$> argVals
    
    inferredType :: Maybe Id
    inferredType = listToMaybe $
      (maybeToList . classForValue . (view _2)) =<<
        (filter ((== SelfArg) . (view _1)) argVals)

    inferSelfArg :: Id -> (Arg, Value) -> Scoper Value
    inferSelfArg cname (SelfArg, value) = applyInferredType cname value
    inferSelfArg _ (_, value) = pure value

applyInferredType :: Id -> Value -> Scoper Value
applyInferredType cname (VInferred ifname _ iparams) = do
  impls <- implForClass cname ifname
  evaluateCases impls iparams
applyInferredType _ value = pure value

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
    Nothing      -> stackTrace $
      "No function defined for arguments (" <> intercalate ", " (show <$> params) <> ")"

funCaseMatchesParams :: [Value] -> FunctionCase -> Bool
funCaseMatchesParams params fcase =
  all (uncurry argMatchesParam) $ zip (fcaseArgs fcase) params

argMatchesParam :: Arg -> Value -> Bool
argMatchesParam (IdArg _) _ = True
argMatchesParam (TypedIdArg _ t) (VTypeInstance cname _ _) = t == cname
argMatchesParam (TypedIdArg _ "Int") (VInt _) = True
argMatchesParam (TypedIdArg _ "Char") (VChar _) = True
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
runFun og@(VTypeCons className consName cargs) params =
  curryWrapper
    og
    (pure $ VTypeInstance className consName params)
    ("Too many args passed to type cons: " <> consName)
    params
runFun expr params = runScopedFun expr params

handleCurrying :: Int -> [Value] -> Value -> Value
handleCurrying expectedLen params thingToCurry = 
    VFunction [
        generateInteropCase newArgs
          (\rest -> runFun thingToCurry (newParams <> rest))
      ]
  where
    actualLen   = length params
    newArgs     = (IdArg . (<> "#") . show) <$> [1..diff]
    diff        = expectedLen - actualLen
    newParams   = take (diff + 1) params

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
    argLenForFuncLike (VFunction (x:_)) =
      length $ fcaseArgs x
    argLenForFuncLike v = trace ("Not a func-like: " <> show v) undefined

runScopedFun :: Value -> [Value] -> Scoper Value
runScopedFun og@(VFunction cases) params =
  curryWrapper
    og
    (evaluateCases cases params)
    "Too many args passed to function"
    params
runScopedFun og@(VTypeFunction defSig tcases) params =
    curryWrapper
      og
      evaluator
      "Too many args passed to function"
      params
  where
    evaluator = if elem SelfArg (getDefSigArgs defSig)
      then evaluateTf defSig cases params
      else pure $ VInferred
                    (getDefSigFunName defSig)
                    (getDefSigTypeName defSig)
                    params
    
    cases = (view _2) <$> tcases

runScopedFun og _ = stackTrace $ "Error: Bad function call (" <> show og <> ")"
