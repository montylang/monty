{-# LANGUAGE TemplateHaskell #-}
module RunnerTypes where

import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Control.Monad.Except
import ParserTypes
import PrettyPrint
import Lens.Micro.Platform
import Text.Megaparsec hiding (Pos)
import Data.IORef (IORef)
import Debug.Trace

type ScopeMap   = HM.HashMap Id Value
type ScopeBlock = IORef ScopeMap
type Scope      = [ScopeBlock]

data ErrVal = ErrString String
  deriving (Show, Eq)

data Context = Context {
  _typeScope :: ScopeMap,
  _scope :: Scope,
  _callStack :: [SourcePos]
}

type Scoper = StateT Context (ExceptT ErrVal IO)

data Type
  = TInt
  | TChar
  | TDouble
  | TUser Id
  | TAnything
  deriving (Show, Eq)

instance PrettyPrint Type where
  prettyPrint TInt      = "int"
  prettyPrint TDouble   = "double"
  prettyPrint TChar     = "char"
  prettyPrint TAnything = "any"
  prettyPrint (TUser t) = t

data FunctionImpl = FunctionImpl
  { fcases :: [FunctionCase],
    ftypeSig :: [Type]
  } deriving (Show, Eq)

instance PrettyPrint FunctionImpl where
  prettyPrint (FunctionImpl cases typeSig) =
    "sig(" <> intercalate ", " (prettyPrint <$> typeSig) <> "):\n" <>
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> cases)

data FunctionCase =
  FunctionCase
      { fcaseArgs :: [Arg],
        fcaseBody :: Scoper Value
      }

instance PrettyPrint FunctionCase where
  prettyPrint (FunctionCase args body) =
    "def(" <> (intercalate ", " $ prettyPrint <$> args) <> "):"

instance Show FunctionCase where
  show fcase = "def (" <> intercalate "," (show <$> args) <> ")"
    where args = fcaseArgs fcase

instance Eq FunctionCase where
  (==) first second = fcaseArgs first == fcaseArgs second

voidValue = VTuple []

data Value
  = VInt Int
  | VDouble Double
  | VChar { vChr :: Char }
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
  | VList { lElements :: [Value] }
  | VDict    
  | VTuple { tElements :: [Value] }
  deriving (Eq)

instance Show Value where
  show (VInt value) = show value
  show (VDouble value) = show value
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
  show (VList values@((VChar _):_)) = show $ vChr <$> values
  show (VList values) = show values
  show (VDict) = undefined
  show (VTuple values) = 
    "(" <> intercalate "," (show <$> values) <> ")"
  show (VInferred fname tname vals) =
    "VInferred " <> fname <> " " <> tname <> " " <> show vals

instance PrettyPrint Value where
  prettyPrint (VInt value) = show value
  prettyPrint (VDouble value) = show value
  prettyPrint (VChar value) = show value
  prettyPrint (VFunction impl) = prettyPrint impl
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
  -- show (VTypeDef name _) = "<type " <> name <> ">"
  -- show (VTypeFunction _ _) = "<function>"
  prettyPrint (VScoped value _) = prettyPrint value
  prettyPrint (VList values@((VChar _):_)) = show $ vChr <$> values
  prettyPrint (VList values) =
    "[" <> intercalate ", " (prettyPrint <$> values) <> "]"
  --show (VClass _) = "<class>"
  -- show (VDict) = undefined
  prettyPrint (VTuple values) = 
    "(" <> intercalate "," (show <$> values) <> ")"
  prettyPrint (VInferred fname tname vals) =
    "(inferreed)" <> fname <> " " <> tname <> " " <> (show $ prettyPrint <$> vals)

$(makeLenses ''Context)
