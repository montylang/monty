module MontyRunner where

import Prelude
import Debug.Trace
import Data.Either
import Data.List
import Data.Maybe
import Control.Monad.State.Strict
import Text.Megaparsec.Pos

import ParserTypes
import RunnerTypes
import CallableUtils
import RunnerUtils
import InstanceUtils (addImplementation)

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
    (VError stack msg) -> runtimeError (sourcePosPretty pos <> ": " <> msg)
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
      Just (VClass consNames, _) -> unError =<< addAllImplementations className consNames funcDefs
      _ -> stackTrace $ "Attempted to use undefined class: " <> className
  where
    functionDefs :: Maybe (Value, Scope) -> Scoper [DefSignature]
    functionDefs (Just (VTypeDef _ sigs, _)) = pure sigs
    functionDefs _ = runtimeError $ "Type " <> typeName <> " not found"

    addAllImplementations :: Id -> [Id] -> [DefSignature] -> Scoper (Either String Value)
    addAllImplementations cname consNames funcDefs = do
      potentialErrors <-
        sequence $ (addImplementation cname consNames funcDefs) <$> getPosValue <$> implementations
      pure $ case sequence potentialErrors of
        Left err -> Left err
        Right _  -> Right $ VInt 0 -- TODO: you know what you've done

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
runFun (VTypeCons className consName cargs) params = pure $
  if (length cargs) == (length params)
    then Right $ VTypeInstance className consName params
    else Left ("Bad type cons call to " <> consName)
runFun expr params = runScopedFun expr params

runScopedFun :: Value -> [Value] -> Scoper (Either String Value)
runScopedFun (VFunction cases) params = evaluateCases cases params
runScopedFun (VTypeFunction _ _ _ cases) params = evaluateCases cases params
runScopedFun _ _ = pure $ Left "Error: Bad function call"

evaluateCases :: [FunctionCase] -> [Value] -> Scoper (Either String Value)
evaluateCases cases params = injectLift runWithTempScope $ do
    fcase <- pickFun cases params
    result <- addArgsToScope (fcaseArgs fcase) params
    case result of
      Left err -> pure $ Left err
      Right _ -> Right <$> (runFcase fcase)
  where
    -- Maybe toooo epic. Beyond illegible
    injectLift :: (Monad mo, Traversable mi, Monad mi) => (mo a -> mo r) -> mo (mi a) -> mo (mi r)
    injectLift f = (=<<) (sequence . (f . pure <$>))

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
