module MontyRunner where

import Prelude
import Debug.Trace
import Control.Monad.State.Strict

import ParserTypes
import RunnerTypes
import CallableUtils
import RunnerUtils

infixEval :: Value -> InfixOp -> Value -> Value
infixEval (VInt first) InfixAdd (VInt second) = VInt $ first + second
infixEval (VInt first) InfixSub (VInt second) = VInt $ first - second
infixEval (VInt first) InfixMul (VInt second) = VInt $ first * second
infixEval (VInt first) InfixEq (VInt second) = VBoolean $ first == second
infixEval _ other _ = trace ("Unimplemented infix: " <> show other) undefined

unError :: Either String Value -> Scoper Value
unError (Left err) = stackTrace err
unError (Right val) = pure val
    
evalP :: PExpr -> Scoper Value
evalP (Pos pos expr) = do
  result <- eval expr

  case result of
    (VError stack message) ->
      runtimeError ("Error: " <> message <> "\n" <> show pos)
    _ -> pure result

eval :: Expr -> Scoper Value
eval (ExprId name) = do
    value <- findInScope name
    case toVScoped <$> value of
      Just val -> pure val
      Nothing  -> stackTrace (name <> " is not in scope")
  where
    toVScoped :: (Value, Scope) -> Value
    -- Only return associated scopes for functions
    toVScoped (VFunction cases, scope) = VScoped (VFunction cases) scope
    toVScoped (value, _) = value

eval (ExprClass className constructors) = do
    addToScope className VClass
    unionTopScope $ convert <$> getPosValue <$> constructors
    pure $ VInt 0 -- TODO: Return... something other than an int :)
  where
    convert :: TypeCons -> (Id, Value)
    convert (TypeCons name args) = (name, VTypeCons name args)

eval (ExprType typeName headers) = do
    addToScope typeName typeDef
    unionTopScope $ addFuncToScope <$> getPosValue <$> headers
    pure $ VInt 0
  where
    typeDef = VTypeDef typeName $ getPosValue <$> headers

    addFuncToScope :: DefSignature -> (Id, Value)
    addFuncToScope (DefSignature tName functionName args) =
      (functionName,  VTypeFunction tName functionName args [])

-- TODO: Ban redefining instances for classes
eval (ExprInstanceOf className typeName implementations) = do
    classDef <- findInScope className
    typeDef  <- findInScope typeName
    funcDefs <- functionDefs typeDef

    scoperAssert (isVClass classDef) $
      "Attempted to use undefined class: " <> className
    
    potentialErrors <- sequence $ (addImplementation funcDefs) <$> getPosValue <$> implementations
    case sequence potentialErrors of
      Left err -> stackTrace err
      Right _  -> pure $ VInt 0 -- TODO: you know what you've done
  where
    isVClass (Just (VClass, _)) = True
    isVClass _                  = False
    
    functionDefs :: Maybe (Value, Scope) -> Scoper [DefSignature]
    functionDefs (Just (VTypeDef _ sigs, _)) = pure sigs
    functionDefs _ = runtimeError $ "Type " <> typeName <> " not found"

    defSigToId :: DefSignature -> Id
    defSigToId (DefSignature _ fname _) = fname
    
    addImplementation :: [DefSignature] -> Expr -> Scoper (Either String ())
    addImplementation avaliable (ExprAssignment name (Pos _ (ExprDef args bod))) = do
        scoperAssert (elem name (defSigToId <$> avaliable)) $
          name <> " is not part of type " <> typeName
        maybeStub <- findInScope name
        getStubOrError maybeStub
      where
        getStubOrError :: Maybe (Value, Scope) -> Scoper (Either String ())
        getStubOrError (Just (stub, _)) =
          Right <$> addToScope name (addToStub (FunctionCase args $ bod) stub)
        getStubOrError Nothing =
          pure $ Left "Ain't in scope biatch"

    addImplementation _ _  =
      pure $ Left "Every root expr in an implementation must be a def"

eval (ExprInt a) = pure $ VInt a
eval (ExprString a) = pure $ VString a
eval (ExprInfix first op second) = do
  f <- evalP first
  s <- evalP second
  pure $ infixEval f op s

eval (ExprIfElse ifCond elifConds elseBody) = do
     selectedBody <- pickBody (ifCond:elifConds)
     evalBody selectedBody
  where
    pickBody :: [CondBlock] -> Scoper [PExpr]
    pickBody [] = pure $ elseBody
    pickBody ((CondBlock condition condBody):xs) = do
      condVal <- evalP condition
      case condVal of
        VBoolean True  -> pure condBody
        VBoolean False -> pickBody xs
        _              -> runtimeError "Condition is not a boolean"

    evalBody :: [PExpr] -> Scoper Value
    evalBody exprs = do
      vals <- sequence $ evalP <$> exprs
      pure $ last vals

eval (ExprList []) = pure $ VList []
eval (ExprList (x:xs)) = do
    headEvaled <- evalP x
    tailEvaled <- sequence $ enforceType (typeOfValue headEvaled) <$> xs
    case sequence tailEvaled of
      Left err   -> stackTrace err
      Right t -> pure $ VList (headEvaled:t)
  where
    enforceType :: String -> PExpr -> Scoper (Either String Value)
    enforceType typeStr expr = do
      evaled <- evalP expr
      pure $ if (typeOfValue evaled) == typeStr
        then Right evaled
        else Left "List must be of the same type"

eval (ExprDef args body) =
  pure $ VFunction [FunctionCase args body]

eval (ExprAssignment name value) = do
    evaledValue  <- evalP value
    inScopeValue <- findInTopScope name
    
    newValue <- case inScopeValue of
      (Just (VFunction cases)) -> unError =<< appendFunctionCase cases evaledValue
      (Just _)                 -> stackTrace $ "Cannot mutate " <> name
      _                        -> pure evaledValue
    
    addToScope name newValue
    pure evaledValue
  where
    appendFunctionCase :: [FunctionCase] -> Value -> Scoper (Either String Value)
    appendFunctionCase cases (VFunction [newCase]) = pure $
      if all (functionCaseFits newCase) cases
        then Left ("Invalid pattern match for function " <> name)
        else Right $ VFunction (cases <> [newCase])
    appendFunctionCase _ _ = pure $ Left $ "Cannot mutate " <> name

    functionCaseFits :: FunctionCase -> FunctionCase -> Bool
    functionCaseFits newCase existingCase =
        (length newCaseArgs == length existingCaseArgs) &&
          (any (uncurry argFits) (zip newCaseArgs existingCaseArgs))
      where
        newCaseArgs = fcaseArgs newCase
        existingCaseArgs = fcaseArgs existingCase

    argFits :: Arg -> Arg -> Bool
    argFits _ (IdArg _) = False
    argFits (IdArg _) (PatternArg _ _) = True
    argFits (PatternArg newName newArgs) (PatternArg existingName existingArgs) =
      newName /= existingName || all (uncurry argFits) (zip newArgs existingArgs)

eval (ExprCall funExpr args) = do
    fun        <- evalP funExpr
    evaledArgs <- sequence $ evalP <$> args
    result     <- runFun fun evaledArgs
    unError result

eval other = stackTrace ("Error (unimplemented expr eval): " <> show other)

runFun :: Value -> [Value] -> Scoper (Either String Value)
runFun (VScoped func fscope) params = do
  result <- runScopedFun func params
  case result of
    Left err  -> pure $ Left err
    Right val -> Right <$> runWithScope fscope (pure val)
runFun (VTypeCons name cargs) params = pure $
  if (length cargs) == (length params)
    then Right $ VTypeInstance name params
    else Left ("Bad type cons call to " <> name)
runFun expr params = runScopedFun expr params

runScopedFun :: Value -> [Value] -> Scoper (Either String Value)
runScopedFun (VFunction cases) params = Right <$> evaluateCases cases params
runScopedFun (VTypeFunction _ _ _ cases) params = Right <$> evaluateCases cases params
runScopedFun _ _ = pure $ Left "Error: Bad function call"

evaluateCases :: [FunctionCase] -> [Value] -> Scoper Value
evaluateCases cases params = runWithTempScope $ do
    fcase <- pickFun cases params
    addArgsToScope (fcaseArgs fcase) params
    runFcase fcase

runFcase :: FunctionCase -> Scoper Value
runFcase (FunctionCase _ body) = do
    _            <- sequence $ evalP <$> beginning
    scope        <- get
    evaledReturn <- evalP returnExpr
    pure $ case evaledReturn of
      (VFunction _) -> VScoped evaledReturn scope
      _             -> evaledReturn
  where
    (beginning, returnExpr) = splitReturn body
runFcase (InteropCase _ body) = body
