module MontyRunner where

import Prelude
import Debug.Trace
import Data.List (intercalate)
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Control.Monad.State.Lazy
import System.Exit

import MontyParser

data TypeClass
  = TAny
  | TInt
  | TString
  deriving (Show, Eq)

data ScopeKey
  = VariableKey String
  -- | FuncSigKey String [TypeClass]
  deriving (Show, Eq)

instance Hashable ScopeKey where
  hashWithSalt salt (VariableKey name) = hashWithSalt salt name

  -- hashWithSalt salt (FuncSigKey name types) =
  --   hashWithSalt salt (intercalate "," ([name] ++ (show <$> types)))

type Scope = HM.HashMap ScopeKey Value
type Scoper a = StateT Scope IO a

data Value
  = VInt Int
  | VString String
  | VBoolean Bool
  | VFunction [Arg] [Expr] Scope -- Parametric
  | VList     -- Parametric
  | VTuple    -- Parametric
  | VDict     -- Parametric
  | VClass    -- Parametric
  deriving (Show)

infixEval :: Value -> InfixOp -> Value -> Value
infixEval (VInt first) InfixAdd (VInt second) = VInt $ first + second
infixEval (VInt first) InfixMul (VInt second) = VInt $ first * second

showAScope :: String -> Scope -> Scoper ()
showAScope prefix scope = do
  _ <- lift $ putStrLn (prefix <> ": " <> (intercalate ", " ((\(VariableKey a) -> a) <$> HM.keys scope)))
  pure ()

showScope :: String -> Scoper ()
showScope prefix = do
  s <- get
  showAScope prefix s

showFScope :: String -> Scoper ()
showFScope prefix = do
    s <- get
    _ <- lift $ (putStrLn $ (prefix <> ": \n" <> (intercalate "\n" $ display <$> (HM.toList s)) <> "\n"))
    pure ()
  where
    display :: (ScopeKey, Value) -> String
    display ((VariableKey name), (VFunction _ _ fscope)) = 
      "Func (" <> name <> "): " <> (intercalate ", " ((\(VariableKey a) -> a) <$> HM.keys fscope))
    display ((VariableKey name), _) = "Value: " <> name 

eval :: Expr -> Scoper Value
eval (ExprId name) = do
  -- showScope $ "id (" <> name <> ")"
  value <- gets (\s -> HM.lookup (VariableKey name) s)
  pure $ fromMaybe (trace (name <> " is not in scope") undefined) value -- TODO: Have a better message

eval (ExprInt a) = pure $ VInt a
eval (ExprString a) = pure $ VString a
eval (ExprInfix first op second) = do
  f <- eval first
  s <- eval second
  pure $ infixEval f op s

eval (ExprDef args body) = do
  showScope "def"
  scope <- get
  showFScope "???"
  pure $ VFunction args body scope

eval (ExprAssignment name value) = do
  showScope ("ass (" <> name <> ")")
  evaledValue <- eval value
  modify (\s -> HM.insert (VariableKey name) evaledValue s)
  pure evaledValue

eval (ExprCall (ExprId "debug") [param]) = do
  evaled <- eval param
  _ <- lift $ (putStrLn $ show evaled)
  pure evaled

eval (ExprCall funExpr args) = do
    fun        <- eval funExpr
    evaledArgs <- sequence $ eval <$> args
    oldState   <- get
    _          <- put HM.empty
    result     <- runFun fun evaledArgs
    _          <- put oldState
    pure result
  where
    runFun :: Value -> [Value] -> Scoper Value
    runFun (VFunction fargs body parentScope) params | (length args) == (length params) = do
      --loadFunctionsIntoScope body
      showScope "load fun"
      showAScope "parent" parentScope
      put $ HM.union (HM.fromList $ zip (convertArg <$> fargs) params) parentScope
      showScope "load rest"
      runBody body
    -- FIXME: complete hack
    runFun _ _ = trace ("Error: Bad function call on line TODO") undefined

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

eval other = trace ("Debug: " <> show other) $ pure $ VString "Catch all. Much bad."

mapArgsToTypes :: [Arg] -> [TypeClass]
mapArgsToTypes args = (\_ -> TAny) <$> args

mergeScoper :: Scope -> Scoper ()
mergeScoper newValues = modify (\s -> HM.union s newValues)
  --   _ <- newScoper
  --   newScope <- get
  --   _        <- modify (\s -> (whatever newScope) <$> s)
  --   pure ()
  -- where 
  --   newScoper :: Scoper ()
  --   newScoper = modify (\s -> HM.union s newValues)

  --   whatever :: Scope -> Value -> Value
  --   whatever scope (VFunction args body _) = VFunction args body scope
  --   whatever _ other = other

loadFunctionsIntoScope :: [Expr] -> Scoper ()
loadFunctionsIntoScope exprs = mergeScoper newScopeEntries
  where
    newScopeEntries :: Scope
    newScopeEntries = HM.fromList [x | Just x <- (tuplify <$> exprs)] -- Decently epic

    tuplify :: Expr -> Maybe (ScopeKey, Value)
    tuplify (ExprAssignment name (ExprDef args body)) =
      -- Technically not optimal as the args and body will be overwritten with the exact same content
      -- but it's such a small issue that chances are nobody will ever notice again
      --Just ((FuncSigKey name $ mapArgsToTypes args), (VFunction args body HM.empty)) 
      Just ((VariableKey name), (VFunction args body HM.empty))
    tuplify _ = Nothing

runs :: [Expr] -> Scoper ()
runs exprs = do
  loadFunctionsIntoScope exprs
  _ <- sequence $ eval <$> exprs
  pure ()

run :: [Expr] -> IO ()
run exprs = evalStateT (runs exprs) HM.empty
