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

eval :: Expr -> Scoper Value
eval (ExprId name) = do
    value <- findInScope name
    case toVScoped <$> value of
      Just val -> pure val
      Nothing  -> runtimeError (name <> " is not in scope")
  where
    toVScoped :: (Value, Scope) -> Value
    -- Only return associated scopes for functions
    toVScoped (VFunction cases, scope) = VScoped (VFunction cases) scope
    toVScoped (value, _) = value

eval (ExprClass className constructors) = do
    addToScope className VClass
    unionTopScope $ convert <$> constructors
    pure $ VInt 0 -- TODO: Return... something other than an int :)
  where
    convert :: TypeCons -> (Id, Value)
    convert (TypeCons name args) = (name, VTypeCons name args)

eval (ExprType typeName headers) = do
    addToScope typeName typeDef
    unionTopScope $ addFuncToScope <$> headers
    pure $ VInt 0
  where
    typeDef = VTypeDef typeName headers

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
    
    _ <- sequence $ (addImplementation funcDefs) <$> implementations
    
    pure $ VInt 0 -- TODO: you know what you've done
  where
    isVClass (Just (VClass, _)) = True
    isVClass _                  = False
    
    functionDefs :: Maybe (Value, Scope) -> Scoper [DefSignature]
    functionDefs (Just (VTypeDef _ sigs, _)) = pure sigs
    functionDefs _ = runtimeError $ "Type " <> typeName <> " not found"

    defSigToId :: DefSignature -> Id
    defSigToId (DefSignature _ fname _) = fname
    
    addImplementation :: [DefSignature] -> Expr -> Scoper ()
    addImplementation avaliable (ExprAssignment name (ExprDef args bod)) = do
      scoperAssert (elem name (defSigToId <$> avaliable)) $
        name <> " is not part of type " <> typeName
      maybeStub <- findInScope name
      stub      <- getStubOrDie maybeStub
      addToScope name (addToStub (FunctionCase args bod) stub)
    addImplementation _ _  = runtimeError "Every root expr in an implementation must be a def"

    getStubOrDie :: Maybe (Value, Scope) -> Scoper Value
    getStubOrDie (Just (val, _)) = pure val
    getStubOrDie Nothing = runtimeError "Ain't in scope biatch"

eval (ExprInt a) = pure $ VInt a
eval (ExprString a) = pure $ VString a
eval (ExprInfix first op second) = do
  f <- eval first
  s <- eval second
  pure $ infixEval f op s

eval (ExprIfElse ifCond elifConds elseBody) = do
     selectedBody <- pickBody (ifCond:elifConds)
     evalBody selectedBody
  where
    pickBody :: [CondBlock] -> Scoper [Expr]
    pickBody [] = pure elseBody
    pickBody ((CondBlock condition condBody):xs) = do
      condVal <- eval condition
      case condVal of
        VBoolean True  -> pure condBody
        VBoolean False -> pickBody xs
        _              -> runtimeError "Condition is not a boolean"

    evalBody :: [Expr] -> Scoper Value
    evalBody exprs = do
      vals <- sequence $ eval <$> exprs
      pure $ last vals

eval (ExprList []) = pure $ VList []
eval (ExprList (x:xs)) = do
    headEvaled <- eval x
    tailEvaled <- sequence $ enforceType (typeOfValue headEvaled) <$> xs
    pure $ VList (headEvaled:tailEvaled)
  where
    enforceType :: String -> Expr -> Scoper Value
    enforceType typeStr expr = do
      evaled <- eval expr
      if (typeOfValue evaled) == typeStr
        then pure evaled
        else runtimeError "List must be of the same type"

eval (ExprDef args body) = pure $ VFunction [FunctionCase args body]

eval (ExprAssignment name value) = do
    evaledValue  <- eval value
    inScopeValue <- findInTopScope name
    
    newValue <- case inScopeValue of
      (Just (VFunction cases)) -> appendFunctionCase cases evaledValue
      (Just _)                 -> runtimeError $ "Cannot mutate " <> name
      _                        -> pure evaledValue
    
    addToScope name newValue
    pure evaledValue
  where
    appendFunctionCase :: [FunctionCase] -> Value -> Scoper Value
    appendFunctionCase cases (VFunction [newCase]) = do
      scoperAssert (all (functionCaseFits newCase) cases) $
        ("Invalid pattern match for function " <> name)
      pure $ VFunction (cases <> [newCase])
    appendFunctionCase _ _ = runtimeError $ "Cannot mutate " <> name

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
    fun        <- eval funExpr
    evaledArgs <- sequence $ eval <$> args
    result     <- runFun fun evaledArgs
    pure result

eval other = runtimeError ("Error (unimplemented expr eval): " <> show other)

runFun :: Value -> [Value] -> Scoper Value
runFun (VScoped func fscope) params =
  runWithScope fscope $ runScopedFun func params
runFun (VTypeCons name cargs) params =
  if (length cargs) == (length params)
    then pure $ VTypeInstance name params
    else runtimeError ("Bad type cons call to " <> name)
runFun expr params = runScopedFun expr params

runScopedFun :: Value -> [Value] -> Scoper Value
runScopedFun (VFunction cases) params = evaluateCases cases params
runScopedFun (VTypeFunction _ _ _ cases) params = evaluateCases cases params
runScopedFun _ _ = runtimeError "Error: Bad function call"

evaluateCases :: [FunctionCase] -> [Value] -> Scoper Value
evaluateCases cases params = runWithTempScope $ do
    fcase <- pickFun cases params
    addArgsToScope (fcaseArgs fcase) params
    runFcase fcase

runFcase :: FunctionCase -> Scoper Value
runFcase (FunctionCase _ body) = do
    _            <- sequence $ eval <$> beginning
    scope        <- get
    evaledReturn <- eval returnExpr
    pure $ case evaledReturn of
      (VFunction _) -> VScoped evaledReturn scope
      _             -> evaledReturn
  where
    (beginning, returnExpr) = splitReturn body
runFcase (InteropCase _ body) = body
