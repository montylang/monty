module CallableUtils (runFun, evaluateImpl, applyInferredType) where

import Prelude
import Data.List
import Data.Maybe
import Lens.Micro.Platform
import Debug.Trace
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Monad.State.Strict

import RunnerUtils
import RunnerTypes
import ParserTypes
import PrettyPrint

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
          (v@(VInferred _ _ _), True) -> applyInferredType cname v
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
      (maybeToList . classForValue . (view _2)) =<<
        (filter ((== SelfArg) . (view _1)) argVals)
    
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

inferTypeValue :: Type -> Value -> Scoper Value
inferTypeValue (TUser tid) v@(VTypeInstance tname _ _) | tid == tname = pure v
inferTypeValue (TUser tid) v@(VInferred fname _ vals) =
  tryInferral tid fname vals
inferTypeValue TAnything v = pure v
inferTypeValue TInt v@(VInt _)   = pure v
inferTypeValue TChar v@(VChar _) = pure v
inferTypeValue (TUser "List") v@(VList _) = pure v
inferTypeValue t v = stackTrace $ "I shit my pants! " <>
  show t <> "," <> prettyPrint v

evaluateImpl :: FunctionImpl -> [Value] -> Scoper Value
evaluateImpl (FunctionImpl cases typeSig) values = do
  inferred <- sequence $ (uncurry inferTypeValue) <$> zip typeSig values
  evaluateCases cases inferred

evaluateCases :: [FunctionCase] -> [Value] -> Scoper Value
evaluateCases cases params = runWithTempScope $ do
  fcase <- pickFun cases params

  assert (length (fcaseArgs fcase) == length params) $
    "Mismatched argument length. Got: " <>
    show (prettyPrint <$> params) <> ", but expected: " <>
    show (prettyPrint <$> fcaseArgs fcase) <> ". Oh and by the way: " <>
    show (prettyPrint <$> cases)
  
  sequence_ $ addArg <$> zip (fcaseArgs fcase) params

  runFcase fcase

runFcase :: FunctionCase -> Scoper Value
runFcase (FunctionCase _ body) = do
    sequence_ $ evalP <$> beginning
    evalP returnExpr
  where
    (beginning, returnExpr) = splitReturn body

runFcase (InteropCase _ body) = body

pickFun :: [FunctionCase] -> [Value] -> Scoper FunctionCase
pickFun cases params = do

  case find (funCaseMatchesParams params) cases of
    Just funCase -> pure funCase 
    Nothing      -> stackTrace $
      "No function defined for arguments (" <>
      intercalate ", " (show <$> params) <>
      ") in " <> show cases

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
    VFunction $ FunctionImpl [
        generateInteropCase newArgs
          (\rest -> runFun thingToCurry (newParams <> rest))
      ] newTypes
  where
    actualLen   = length params
    newArgs     = (IdArg . (<> "#") . show) <$> [1..diff]
    diff        = expectedLen - actualLen
    newParams   = take (diff + 1) params
    newTypes    = drop actualLen $ extractTypeSig thingToCurry

    extractTypeSig :: Value -> [Type]
    extractTypeSig (VFunction (FunctionImpl _ types)) = types
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
    argLenForFuncLike (VFunction (FunctionImpl (x:_) _)) =
      length $ fcaseArgs x
    argLenForFuncLike v = trace ("Not a func-like: " <> show v) undefined

runScopedFun :: Value -> [Value] -> Scoper Value
runScopedFun og@(VFunction impl) params =
  curryWrapper
    og
    (evaluateImpl impl params)
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
      then evaluateTf defSig tcases params
      else pure $ VInferred
                    (getDefSigFunName defSig)
                    (getDefSigTypeName defSig)
                    params

runScopedFun og _ = stackTrace $ "Error: Bad function call (" <> show og <> ")"
