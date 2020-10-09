{-# LANGUAGE TemplateHaskell #-}
module RunnerTypes where

import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Monad.Except
import ParserTypes
import Lens.Micro.Platform
import Text.Megaparsec hiding (Pos)

type ScopeBlock = HM.HashMap Id Value
type Scope      = [ScopeBlock]

data ErrVal = ErrString String
  deriving (Show, Eq)

data Executors = Executors {
  _evaluatePExpr :: PExpr -> Scoper Value,
  _evaluateExpr :: Expr -> Scoper Value
}

data Context = Context {
  _scope :: Scope,
  _executors :: Executors,
  _callStack :: [SourcePos]
}

type Scoper = StateT Context (ExceptT ErrVal IO)

data Type
  = TInt
  | TChar
  | TUser Id
  | TAnything
  deriving (Show, Eq)

data FunctionImpl = FunctionImpl
  { fcases :: [FunctionCase],
    ftypeSig :: [Type]
  } deriving (Show, Eq)

data FunctionCase
  = FunctionCase
      { fcaseArgs :: [Arg],
        fcaseBody :: [PExpr]
      }
  | InteropCase
      { fcaseArgs :: [Arg],
        fcaseInteropBody :: (Scoper Value)
      }

instance Show FunctionCase where
  show fcase = "def (" <> intercalate "," (show <$> args) <> ")"
    where args = fcaseArgs fcase

instance Eq FunctionCase where
  (==) first second = fcaseArgs first == fcaseArgs second

data Value
  = VInt Int
  | VChar { chr :: Char }
  | VCurried Value [Value]
  | VFunction FunctionImpl
  | VInferred {
      iFuncName :: Id,
      iTypeName :: Id,
      iValues :: [Value]
    }
  | VTypeCons {
      getVTypeConsClass :: Id,
      getVTypeConsName :: Id,
      getVTypeConsArgs :: [Id]
    }
  | VTypeInstance {
      getTypeInstanceClass :: Id,
      getTypeInstanceName :: Id,
      getTypeInstanceVals :: [Value]
    }
  -- Name, [Arg]
  -- Arg must contain _one_ instance of the word "self", for pattern matching
  | VTypeDef Id [DefSignature]
  | VTypeFunction {
      fDefSig :: DefSignature,
      fFuncCases :: HM.HashMap Id FunctionImpl
    }
  | VScoped Value Scope
  | VClass [Id]
  | VList [Value]
  | VDict    
  | VTuple   
  deriving (Eq)

instance Show Value where
  show (VInt value) = show value
  show (VChar value) = show value
  show (VFunction cases) = "fun:" <> show cases
  show (VTypeCons _ name args) =
    name <>
    if length args == 0
      then ""
      else "(" <> intercalate "," args <> ")"
  show (VTypeInstance _ name vals) =
    name <>
    if length vals == 0
      then ""
      else "(" <> intercalate "," (show <$> vals) <> ")"
  show (VTypeDef name _) = "<type " <> name <> ">"
  show (VTypeFunction _ _) = "<function>"
  show (VScoped value _) = show value
  show (VClass _) = "<class>"
  show (VList values@((VChar _):_)) = show $ chr <$> values
  show (VList values) = show values
  show (VDict) = undefined
  show (VTuple) = undefined
  show (VInferred fname tname vals) =
    "VInferred " <> fname <> " " <> tname <> " " <> show vals

prettyPrint :: Value -> String
prettyPrint (VInt value) = show value
prettyPrint (VChar value) = show value
prettyPrint (VFunction _) = "<function>"
prettyPrint (VTypeCons _ name args) =
  name <>
  if length args == 0
    then ""
    else "(" <> intercalate "," args <> ")"
prettyPrint (VTypeInstance _ name vals) =
  name <>
  if length vals == 0
    then ""
    else "(" <> intercalate "," (prettyPrint <$> vals) <> ")"
prettyPrint (VTypeDef name _) = "<type " <> name <> ">"
prettyPrint (VTypeFunction _ _) = "<function>"
prettyPrint (VScoped value _) = show value
prettyPrint (VClass _) = "<class>"
prettyPrint (VList values@((VChar _):_)) = show $ chr <$> values
prettyPrint (VList values) = show values
prettyPrint (VDict) = undefined
prettyPrint (VTuple) = undefined
prettyPrint (VInferred fname tname vals) =
  "VInferred " <> fname <> " " <> tname <> " " <> show vals

$(makeLenses ''Executors)
$(makeLenses ''Context)
