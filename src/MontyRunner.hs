module MontyRunner where

import Prelude
import Debug.Trace
import Data.List (intercalate, find)
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Monad.State.Strict
import System.Exit

import MontyParser

data TypeClass
  = TAny
  | TInt
  | TString
  deriving (Show, Eq)

type ScopeBlock = HM.HashMap Id Value
type Scope      = [ScopeBlock]

type Scoper a = StateT Scope IO a

data FunctionCase = FunctionCase [Arg] [Expr]
  deriving (Show, Eq)

data Value
  = VInt Int
  | VString String
  | VBoolean Bool
  | VFunction [FunctionCase] -- Parametric
  | VTypeCons Id [Id]
  | VTypeInstance Id [Value] -- Id = Type name
  | VScoped Value Scope
  | VClass String [Value] -- Parametric
  | VList     -- Parametric
  | VDict     -- Parametric
  | VTuple    -- Parametric
  deriving (Show, Eq)

argToId :: Arg -> Id
argToId (IdArg name) = name
argToId _ = trace "you bad boi, you" undefined

-- TODO: Don't allow overriding of values in top scope
addToScope :: String -> Value -> Scope -> Scope
addToScope key value (topScope:lowerScopes) = newTop:lowerScopes
  where
    newTop = HM.insert key value topScope
addToScope _ _ [] = undefined

unionTopScope :: ScopeBlock -> Scope -> Scope
-- TODO: Error on name collisions
unionTopScope new (topScopeBlock:bottomBlocks) =
  (HM.union new topScopeBlock):bottomBlocks
unionTopScope _ [] = undefined

-- Returns the value for the given key, and the scope block where it is defined
findInScope :: String -> Scope -> Maybe (Value, Scope)
findInScope _ [] = Nothing
findInScope key (top:lower) =
  case HM.lookup key top of
    Nothing    -> findInScope key lower
    Just value -> Just (value, top:lower)

pushScopeBlock :: ScopeBlock -> Scope -> Scope
pushScopeBlock block scope = block:scope

popScopeBlock :: Scope -> Scope
popScopeBlock [] = []
popScopeBlock (_:bottom) = bottom

infixEval :: Value -> InfixOp -> Value -> Value
infixEval (VInt first) InfixAdd (VInt second) = VInt $ first + second
infixEval (VInt first) InfixSub (VInt second) = VInt $ first - second
infixEval (VInt first) InfixMul (VInt second) = VInt $ first * second
infixEval (VInt first) InfixEq (VInt second) = VBoolean $ first == second
infixEval _ other _ = trace ("Unimplemented infix: " <> show other) undefined

runtimeError :: String -> Scoper a
runtimeError message = do
  _ <- lift $ die message
  -- Will never get reached, but hey, it fixes compiler errors
  undefined

scoperAssert :: Bool -> String -> Scoper ()
scoperAssert False message = runtimeError message
scoperAssert True _ = pure ()

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

eval (ExprClass _ constructors) = do
    modify (\s -> unionTopScope (HM.fromList (convert <$> constructors)) s)
    pure $ VInt 0 -- TODO: Return... something other than an int :)
  where
    convert :: TypeCons -> (Id, Value)
    convert (TypeCons name args) =
      (name, VTypeCons name (argToId <$> args))

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

eval (ExprList []) = pure $ VTypeInstance "Nil" []
eval (ExprList (x:xs)) = do
  headEvaled <- eval x
  tailEvaled <- eval $ ExprList xs
  pure $ VTypeInstance "Cons" [headEvaled, tailEvaled]

eval (ExprDef args body) = pure $ VFunction [FunctionCase args body]

eval (ExprAssignment name value) = do
    evaledValue  <- eval value
    inScopeValue <- gets (\s -> HM.lookup name $ head s)
    
    newValue <- case inScopeValue of
      (Just (VFunction cases)) -> appendFunctionCase cases evaledValue
      (Just _)                 -> runtimeError $ "Cannot mutate " <> name
      _                        -> pure evaledValue
    
    modify (\s -> addToScope name newValue s)
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
    runScopedFun (VFunction cases) params = do
      (FunctionCase fargs body) <- pickFun cases params
      modify (\s -> pushScopeBlock HM.empty s)
      bazinga fargs params
      retVal <- runBody body
      modify (\s -> popScopeBlock s)
      pure retVal
    runScopedFun _ _ = runtimeError "Error: Bad function call on line TODO"

    pickFun :: [FunctionCase] -> [Value] -> Scoper FunctionCase
    pickFun cases params = do
      case find (funCaseMatchesParams params) cases of
        Just funCase -> pure funCase 
        -- FIXME: Better error message
        Nothing      -> runtimeError $ "No function defined for " <> show params

    funCaseMatchesParams :: [Value] -> FunctionCase -> Bool
    funCaseMatchesParams params (FunctionCase fargs _) =
      all argMatchesParams $ zip fargs params

    argMatchesParams :: (Arg, Value) -> Bool
    argMatchesParams ((IdArg _), _) = True
    argMatchesParams ((PatternArg pname _), (VTypeInstance tname _)) =
      pname == tname
    argMatchesParams _ = False

    runBody :: [Expr] -> Scoper Value
    runBody exprs = do
        -- showScope "run body"
        _ <- sequence $ eval <$> beginning
        eval returnExpr
      where 
        (beginning, returnExpr) = splitReturn exprs

    splitReturn :: [Expr] -> ([Expr], Expr)
    splitReturn exprs =
      let (beginning, [ExprReturn returnExpr]) = splitAt ((length exprs) - 1) exprs in
        (beginning, returnExpr)

    bazinga :: [Arg] -> [Value] -> Scoper ()
    bazinga fargs values = do
      scoperAssert (length fargs == length values) "Mismatched argument length"
      _ <- sequence $ addArg <$> (zip fargs values)
      pure ()

    addArg :: (Arg, Value) -> Scoper ()
    addArg ((IdArg name), v) = modify (\s -> addToScope name v s)
    addArg ((PatternArg pname pargs), (VTypeInstance tname tvals)) = do
      scoperAssert (pname == tname)
        $ "Mismatched pattern match: " <> pname <> "," <> tname
      scoperAssert (length pargs == length tvals)
        $ "Mismatched argument length for pattern match of " <> pname
      modify (\s -> unionTopScope (HM.fromList (zip (argToId <$> pargs) tvals)) s)
      pure ()
    addArg _ =
      runtimeError $ "Bad call to pattern matched function " <> show funExpr

eval other = runtimeError ("Error (unimplemented expr eval): " <> show other)

mapArgsToTypes :: [Arg] -> [TypeClass]
mapArgsToTypes args = (\_ -> TAny) <$> args

runs :: [Expr] -> Scoper ()
runs exprs = do
  _ <- sequence $ eval <$> exprs
  pure ()

run :: [Expr] -> IO ()
run exprs = evalStateT (runs exprs) [HM.empty]
