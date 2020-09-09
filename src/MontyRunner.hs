module MontyRunner where

import Prelude
import Debug.Trace
import Data.List (intercalate)
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

-- TODO: Scope blocks - List of scopes

data ScopeKey
  = VariableKey String
  -- | FuncSigKey String [TypeClass]
  deriving (Show, Eq)

instance Hashable ScopeKey where
  hashWithSalt salt (VariableKey name) = hashWithSalt salt name

  -- hashWithSalt salt (FuncSigKey name types) =
  --   hashWithSalt salt (intercalate "," ([name] ++ (show <$> types)))

type ScopeBlock = HM.HashMap ScopeKey Value
type Scope      = [ScopeBlock]

type Scoper a = StateT Scope IO a

data Value
  = VInt Int
  | VString String
  | VBoolean Bool
  | VFunction [Arg] [Expr] -- Parametric
  | VScoped Value Scope
  | VList     -- Parametric
  | VTuple    -- Parametric
  | VDict     -- Parametric
  | VClass    -- Parametric
  deriving (Show)

-- TODO: Don't allow overriding of values in top scope
addToScope :: String -> Value -> Scope -> Scope
addToScope key value (topScope:lowerScopes) = newTop:lowerScopes
  where
    newTop = HM.insert (VariableKey key) value topScope
addToScope _ _ [] = undefined

-- Returns the value for the given key, and the scope block where it is defined
findInScope :: String -> Scope -> Maybe (Value, Scope)
findInScope _ [] = Nothing
findInScope key (top:lower) =
  case HM.lookup (VariableKey key) top of
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

eval :: Expr -> Scoper Value
eval (ExprId name) = do
    value <- gets (\s -> findInScope name s)
    case toVScoped <$> value of
      Just val -> pure val
      Nothing  -> runtimeError (name <> " is not in scope")
  where
    toVScoped :: (Value, Scope) -> Value
    -- Only return associated scopes for functions
    toVScoped (VFunction args body, scope) = VScoped (VFunction args body) scope
    toVScoped (value, _) = value

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

eval (ExprDef args body) = pure $ VFunction args body

eval (ExprAssignment name value) = do
  evaledValue <- eval value
  modify (\s -> addToScope name evaledValue s)
  pure evaledValue

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
    runFun expr params = runScopedFun expr params

    runScopedFun :: Value -> [Value] -> Scoper Value
    runScopedFun (VFunction fargs body) params | (length args) == (length params) = do
      modify (\s -> pushScopeBlock (HM.fromList $ zip (convertArg <$> fargs) params) s)
      retVal <- runBody body
      modify (\s -> popScopeBlock s)
      pure retVal
    runScopedFun _ _ = runtimeError "Error: Bad function call on line TODO"

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

    convertArg :: Arg -> ScopeKey
    convertArg (IdArg name) = VariableKey name

eval other = runtimeError ("Error (unimplemented expr eval): " <> show other)

mapArgsToTypes :: [Arg] -> [TypeClass]
mapArgsToTypes args = (\_ -> TAny) <$> args

runs :: [Expr] -> Scoper ()
runs exprs = do
  _ <- sequence $ eval <$> exprs
  pure ()

run :: [Expr] -> IO ()
run exprs = evalStateT (runs exprs) [HM.empty]
