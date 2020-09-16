module MontyRunner where

import Prelude
import Debug.Trace
import qualified Data.HashMap.Strict as HM
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
    value <- gets (\s -> findInScope name s)
    case toVScoped <$> value of
      Just val -> pure val
      Nothing  -> runtimeError (name <> " is not in scope")
  where
    toVScoped :: (Value, Scope) -> Value
    -- Only return associated scopes for functions
    toVScoped (VFunction cases, scope) = VScoped (VFunction cases) scope
    toVScoped (value, _) = value

eval (ExprClass className constructors) = do
    modify (addToScope className VClass)
    modify (\s -> unionTopScope (HM.fromList (convert <$> constructors)) s)
    pure $ VInt 0 -- TODO: Return... something other than an int :)
  where
    convert :: TypeCons -> (Id, Value)
    convert (TypeCons name args) =
      (name, VTypeCons name (argToId <$> args))

eval (ExprType typeName headers) = do
    modify (addToScope typeName typeDef)
    modify (\s -> foldl addFuncToScope s headers)
    pure $ VInt 0
  where
    typeDef = VTypeDef typeName headers

    addFuncToScope :: Scope -> DefSignature -> Scope
    addFuncToScope scope (DefSignature tName functionName args) =
      addToScope functionName (VTypeFunction tName functionName args []) scope

-- TODO: Ban redefining instances for classes
eval (ExprInstanceOf className typeName implementations) = do
    classDef <- gets $ findInScope className
    typeDef  <- gets $ findInScope typeName
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
      maybeStub <- gets $ findInScope name
      stub      <- getStubOrDie maybeStub
      modify (addToScope name (addToStub (FunctionCase args bod) stub))
      pure ()
    addImplementation _ _  = runtimeError "Every root expr in an implementation must be a def"

    getStubOrDie :: Maybe (Value, Scope) -> Scoper Value
    getStubOrDie (Just (val, _)) = pure val
    getStubOrDie Nothing = runtimeError "Ain't in scope biatch"

    -- TODO: Much duplication. Make it not so
    addToStub :: FunctionCase -> Value -> Value
    addToStub newCase (VTypeFunction tname fname args cases) =
      VTypeFunction tname fname args (cases ++ [newCase])
    addToStub _ _ = trace "Rat pies" undefined

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
    inScopeValue <- gets (\s -> HM.lookup name $ head s)
    
    newValue <- case inScopeValue of
      (Just (VFunction cases)) -> appendFunctionCase cases evaledValue
      (Just _)                 -> runtimeError $ "Cannot mutate " <> name
      _                        -> pure evaledValue
    
    modify (addToScope name newValue)
    pure evaledValue
  where
    appendFunctionCase :: [FunctionCase] -> Value -> Scoper Value
    appendFunctionCase cases (VFunction [newCase]) = do
      scoperAssert (all (functionCaseFits newCase) cases) ("Invalid pattern match for function " <> name)
      pure $ VFunction (newCase:cases)
    appendFunctionCase _ _ = runtimeError $ "Cannot mutate " <> name

    functionCaseFits :: FunctionCase -> FunctionCase -> Bool
    functionCaseFits (FunctionCase newCaseArgs _) (FunctionCase existingCaseArgs _) =
      (length newCaseArgs == length existingCaseArgs) &&
        (any argFits (zip newCaseArgs existingCaseArgs))

    argFits :: (Arg, Arg) -> Bool
    argFits (_, IdArg _) = False
    argFits (IdArg _, PatternArg _ _) = True
    argFits (PatternArg newName _, PatternArg existingName _) =
      newName /= existingName

eval (ExprCall (ExprId "debug") [param]) = do
  evaled <- eval param
  lift $ putStrLn $ show evaled
  pure evaled

eval (ExprCall funExpr args) = do
    fun        <- eval funExpr
    evaledArgs <- sequence $ eval <$> args
    result     <- runFun fun evaledArgs
    pure result
  where
    runFun :: Value -> [Value] -> Scoper Value
    runFun (VScoped func fscope) params = do
      callingScope <- get
      put fscope
      retVal <- runScopedFun func params
      put callingScope
      pure retVal
    runFun (VTypeCons name cargs) params = do
      if (length cargs) == (length params)
        then pure $ VTypeInstance name params
        else runtimeError ("Bad type cons call to " <> name)
    runFun expr params = runScopedFun expr params
    
    runScopedFun :: Value -> [Value] -> Scoper Value
    runScopedFun (VFunction cases) params = evaluateCases cases params
    runScopedFun (VTypeFunction _ _ _ cases) params = evaluateCases cases params
    runScopedFun _ _ = runtimeError "Error: Bad function call on line TODO"
    
    evaluateCases :: [FunctionCase] -> [Value] -> Scoper Value
    evaluateCases cases params = do
      (FunctionCase fargs body) <- pickFun cases params
      modify (\s -> pushScopeBlock HM.empty s)
      addArgsToScope fargs params
      retVal <- runBody body
      modify (\s -> popScopeBlock s)
      pure retVal
    
    runBody :: [Expr] -> Scoper Value
    runBody exprs = do
        _ <- sequence $ eval <$> beginning
        eval returnExpr
      where 
        (beginning, returnExpr) = splitReturn exprs

eval other = runtimeError ("Error (unimplemented expr eval): " <> show other)

runs :: [Expr] -> Scoper ()
runs exprs = do
  _ <- sequence $ eval <$> exprs
  pure ()

run :: [Expr] -> IO ()
run exprs = evalStateT (runs exprs) [HM.empty]
