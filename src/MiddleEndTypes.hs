{-# LANGUAGE TemplateHaskell #-}
module MiddleEndTypes where

import qualified Data.HashMap.Strict as HM
import Control.Lens
import ParserTypes (Id, Arg, CondBlock (CondBlock))
import Text.Megaparsec (SourcePos)
import Data.List (intercalate)
import MorphUtils (replace)
import Control.Monad.State (State)

data FuncSig = FuncSig
  { _fargs :: [MType]
  , _ret :: MType }

instance Show FuncSig where
  show (FuncSig args ret) =
    "(" <> intercalate ", " (show <$> args) <> ") -> " <>
    show ret

data FuncDef = FuncDef
  { _name :: Maybe String
  , _sourcePos :: Maybe SourcePos
  , _argCount :: Int
  , _sigs :: [FuncSig] }

instance Show FuncDef where
  show _ = "funcdef"

type FuncTable = HM.HashMap Integer FuncDef

showTable :: FuncTable -> String
showTable table = intercalate "\n" $ uncurry showEntry <$> HM.toList table
  where
    showEntry :: Integer -> FuncDef -> String
    showEntry id def =
      show id <>
      showName def <>
      "[" <> show (_argCount def) <> "]\n" <>
      intercalate "\n" (("  - " <>) . show <$> _sigs def)

    showName :: FuncDef -> String
    showName def = case _name def of
      Just name -> "(" <> name <> ")"
      _         -> "<anon>"

data Env = Env
  { _funcTable :: FuncTable
  }

instance Show Env where
  show (Env funcTable) = "Environment:\n" <> showTable funcTable

emptyFuncTable :: FuncTable
emptyFuncTable = HM.fromList
  [
    (0, FuncDef (Just "#add") Nothing 2
      [
        FuncSig [MInt, MInt] MInt,
        FuncSig [MDouble, MDouble] MDouble
      ])
  ]

emptyEnv = Env emptyFuncTable

data MType
  = MInt
  | MDouble
  | MChar
  --  | MTuple [MType]
  --  | MClass Id
  | MFunction [MType] MType
  | MUnknown

instance Show MType where
  show MInt = "int"
  show MDouble = "double"
  show MChar = "char"
  show MUnknown = "???"
  show (MFunction args ret) = "(" <> sig <> ")"
    where
      argSig = intercalate "," $ show <$> args
      retSig = show ret
      sig = argSig <> " -> " <> retSig

data MExpr
  = MExprId
    { _mpos :: SourcePos
    , _id :: Id }
  | MExprAssignment
    { _mpos :: SourcePos
    , _mtype :: MType
    , _lhs :: Arg
    , _rhs :: MExpr }
  | MExprBlock
    { _mpos :: SourcePos
    , _mtype :: MType
    , _body :: [MExpr] }
  | MExprCall
    { _mpos :: SourcePos
    , _mtype :: MType
    , _callee :: MExpr
    , _params :: [MExpr] }
  | MExprIfElse
    { _mpos :: SourcePos
    , _mtype :: MType
    , _ifCond :: CondBlock MExpr
    , _elifConds :: [CondBlock MExpr]
    , _elseBody :: [MExpr] }
  | MExprInt
    { _mpos :: SourcePos
    , _ivalue :: Int }
  | MExprDouble
    { _mpos :: SourcePos
    , _dvalue :: Double }
  | MExprChar
    { _mpos :: SourcePos
    , _cvalue :: Char }
  | MExprDef -- TODO: Maybe store available type sigs elsewhere
    { _mpos :: SourcePos
    , _signatures :: [MType]
    , _args :: [Arg]
    , _body :: [MExpr] 
    }
  -- TODO: Class, type, instance, tuple

indent :: String -> String
indent = ("  " <>) . replace "\n" "\n  "

instance Show MExpr where
  show (MExprAssignment _ _ lhs rhs) =
    show lhs <> " = " <> show rhs
  show (MExprBlock _ _ body) =
    indent $ intercalate "\n" $ show <$> body
  show (MExprCall _ _ callee params) =
    show callee <> "(" <> intercalate ", " (show <$> params) <> ")"
  show (MExprIfElse _ _ ifCond elifConds elseBody) =
    ifPrinted <> elifsPrinted <> elsePrinted
    where
      ifPrinted    = "if " <> show ifCond
      elifsPrinted = intercalate "\n" (("elif " <>) . show <$> elifConds)
      elsePrinted  = "else:\n" <> indent (intercalate "\n" (show <$> elseBody))
  show (MExprInt _ value)    = show value
  show (MExprDouble _ value) = show value
  show (MExprChar _ value)   = show value
  show (MExprId _ value)     = value
  show (MExprDef _ _ args body) =
    "def (" <> argsPrinted <> "):\n" <> indent bodyPrinted
    where
      argsPrinted = intercalate ", " $ show <$> args
      bodyPrinted = intercalate "\n" $ show <$> body

$(makeLenses ''MExpr)

$(makeLenses ''FuncSig)
$(makeLenses ''FuncDef)
$(makeLenses ''Env)
