module MontyRunner where

import Data.Maybe
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Megaparsec
import System.Exit

import ParserTypes
import RunnerTypes
import CallableUtils
import TypeUtils
import RunnerUtils
import InfixUtils
import InstanceUtils (addImplementation)
import ModuleLoader

genericInfixEval :: Value -> InfixOp -> Value -> Scoper Value
genericInfixEval first InfixEq second =
  applyBinaryFun "equals" first second
genericInfixEval first InfixNe second =
  applyUnaryNot =<< applyBinaryFun "equals" first second
genericInfixEval first InfixGt second = compareOrderable first InfixGt second
genericInfixEval first InfixGe second = compareOrderable first InfixGe second
genericInfixEval first InfixLt second = compareOrderable first InfixLt second
genericInfixEval first InfixLe second = compareOrderable first InfixLe second
genericInfixEval _ op _ = stackTrace ("Unimplemented generic infix " <> show op)

compareOrderable :: Value -> InfixOp -> Value -> Scoper Value
compareOrderable f op s =
  (toBoolValue . opToComp op) <$> applyBinaryFun "compare" f s

applyBinaryFun :: Id -> Value -> Value -> Scoper Value
applyBinaryFun fname f s = do
  impls <- findImplsInScope fname f

  case impls of
    []     -> stackTrace $ "No '" <> fname <> "' implementation for " <> show f
    fcases -> evaluateCases fcases [f, s]

evalP :: PExpr -> Scoper Value
evalP (Pos pos expr) = catchError (eval expr) exitOnError
  where
    exitOnError :: ErrVal -> Scoper Value
    exitOnError (ErrString err) =
      liftIO $ die $ sourcePosPretty pos <> ": " <> err

eval :: Expr -> Scoper Value
eval (ExprId "_") = stackTrace "Cannot use black hole as variable"
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
    addToScope className (VClass consNames)
    unionTopScope $ convert <$> getPosValue <$> constructors
    pure $ VInt 0 -- TODO: Return... something other than an int :)
  where
    convert :: TypeCons -> (Id, Value)
    convert (TypeCons name args) = (name, VTypeCons className name args)

    consNames = getTypeConsName . getPosValue <$> constructors

eval (ExprType typeName headers) = do
    addToScope typeName typeDef
    unionTopScope $ defSigToKeyValue <$> getPosValue <$> headers
    pure $ VInt 0
  where
    typeDef = VTypeDef typeName $ getPosValue <$> headers

    defSigToKeyValue :: DefSignature -> (Id, Value)
    defSigToKeyValue (DefSignature tName functionName args) =
      (functionName,  VTypeFunction tName functionName args [])

-- TODO: Ban redefining instances for classes
eval (ExprInstanceOf className typeName implementations) = do
    classDef <- findInScope className
    typeDef  <- findInScope typeName
    funcDefs <- functionDefs typeDef
    
    case classDef of
      Just (VClass consNames, _) -> addAllImplementations className consNames funcDefs
      _ -> stackTrace $ "Attempted to use undefined class: " <> className
  where
    functionDefs :: Maybe (Value, Scope) -> Scoper [DefSignature]
    functionDefs (Just (VTypeDef _ sigs, _)) = pure sigs
    functionDefs _ = stackTrace $ "Type " <> typeName <> " not found"

    addAllImplementations :: Id -> [Id] -> [DefSignature] -> Scoper Value
    addAllImplementations cname consNames funcDefs = do
      _ <- sequence $ (addImplementation cname consNames funcDefs)
                      <$> getPosValue <$> implementations
      pure $ VInt 0 -- TODO: you know what you've done

eval (ExprInt a) = pure $ VInt a
eval (ExprString a) = pure $ VString a

eval (ExprInfix first op second) = do
  f <- evalP first
  s <- evalP second
  if typesEqual f s
    then case f of
      (VInt _) -> pure $ intInfixEval f op s
      _        ->
        if typesEqual f s
          then genericInfixEval f op s
          else stackTrace "Cannot compare values of different types"
    else stackTrace "Cannot compare values of different types"

eval (ExprIfElse ifCond elifConds elseBody) = do
     selectedBody <- pickBody (ifCond:elifConds)
     evalBody selectedBody
  where
    pickBody :: [CondBlock] -> Scoper [PExpr]
    pickBody [] = pure $ elseBody
    pickBody ((CondBlock condition condBody):xs) = do
      condVal <- evalP condition

      case condVal of
        (VTypeInstance _ "True" _)  -> pure condBody
        (VTypeInstance _ "False" _) -> pickBody xs
        _           -> stackTrace "Condition is not a boolean"

    evalBody :: [PExpr] -> Scoper Value
    evalBody exprs = do
      vals <- sequence $ evalP <$> exprs
      pure $ last vals

eval (ExprList []) = pure $ VList []
eval (ExprList (x:xs)) = do
    headEvaled <- evalP x
    tailEvaled <- sequence $ enforceType headEvaled <$> xs
    pure $ VList (headEvaled:tailEvaled)
  where
    enforceType :: Value -> PExpr -> Scoper Value
    enforceType headVal expr = do
      evaled <- evalP expr
      if typesEqual evaled headVal
        then pure evaled
        else stackTrace "List must be of the same type"

eval (ExprDef args body) =
  pure $ VFunction [FunctionCase args body]

eval (ExprAssignment name value) = do
    evaledValue  <- evalP value
    inScopeValue <- findInTopScope name
    
    newValue <- case inScopeValue of
      (Just (VFunction cases)) -> appendFunctionCase cases evaledValue
      (Just _)                 -> stackTrace $ "Cannot mutate " <> name
      _                        -> pure evaledValue
    
    addToScope name newValue
    pure evaledValue
  where
    appendFunctionCase :: [FunctionCase] -> Value -> Scoper Value
    appendFunctionCase cases (VFunction [newCase]) =
      if all (functionCaseFits newCase) cases
        then stackTrace ("Invalid pattern match for function " <> name)
        else pure $ VFunction (cases <> [newCase])
    appendFunctionCase _ _ = stackTrace $ "Cannot mutate " <> name

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
    runFun fun evaledArgs

eval (ExprUnwrap content) = do
    evalUnwrap' Nothing content
  where
    evalUnwrap' :: Maybe Id -> [PExpr] -> Scoper Value
    evalUnwrap' (Just className) [Pos _ (ExprWrap result)] = do
      evaledResult <- evalP result
      impls <- findImplsInScope "wrap" evaledResult

      case impls of
        []     -> stackTrace $ "No wrap implementation for " <> className
        fcases -> evaluateCases fcases [evaledResult]

    evalUnwrap' _ [_] =
      stackTrace "Last statement in unwrap must be wrap"

    evalUnwrap' (Just className) ((Pos _ (ExprBind var expr)):xs) = do
      evaled <- evalP expr

      if (Just className) == classForValue evaled
        then unwrapWithClassname className var evaled xs
        else stackTrace $
          "All binds in unwrap must be of same monad type. " <>
          "Expected '" <> className <> "' got '" <>
          (fromMaybe "<primitive>" (classForValue evaled)) <>
          "'"

    evalUnwrap' Nothing ((Pos _ (ExprBind var expr)):xs) = do
      evaledExpr <- evalP expr

      case evaledExpr of
        (VList _)                     -> unwrapWithClassname "List" var evaledExpr xs
        (VTypeInstance className _ _) -> unwrapWithClassname className var evaledExpr xs
        _ -> stackTrace "Initial expr in unwrap is not monadic"

    evalUnwrap' _ _ = stackTrace "something terrible has happened"

    unwrapWithClassname :: Id -> Id -> Value -> [PExpr] -> Scoper Value
    unwrapWithClassname className var expr xs = do
      lambda <- pure $ generateInteropCase [IdArg var] $
        const $ evalUnwrap' (Just className) xs

      impls <- findImplsInScope "bind" expr

      case impls of
        []     -> stackTrace $ "No bind implementation for " <> className
        fcases -> evaluateCases fcases [expr, VFunction [lambda]]

eval (ExprImport components) = do
  loadModule evalP components
  pure $ VInt 0

eval other = stackTrace ("Error (unimplemented expr eval): " <> show other)

runFun :: Value -> [Value] -> Scoper Value
runFun (VScoped func fscope) params = do
  result <- runScopedFun func params
  runWithScope fscope (pure result)
runFun (VTypeCons className consName cargs) params =
  if (length cargs) == (length params)
    then pure $ VTypeInstance className consName params
    else stackTrace ("Bad type cons call to " <> consName)
runFun expr params = runScopedFun expr params

runScopedFun :: Value -> [Value] -> Scoper Value
runScopedFun (VFunction cases) params = evaluateCases cases params
runScopedFun (VTypeFunction _ _ _ tcases) params = evaluateCases cases params
  where
    cases = (\(_, c) -> c) <$> tcases
runScopedFun _ _ = stackTrace "Error: Bad function call"

evaluateCases :: [FunctionCase] -> [Value] -> Scoper Value
evaluateCases cases params = runWithTempScope $ do
  fcase <- pickFun cases params
  _     <- addArgsToScope (fcaseArgs fcase) params
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
