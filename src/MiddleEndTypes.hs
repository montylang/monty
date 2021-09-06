{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module MiddleEndTypes where

import Control.Lens
import ParserTypes (Id, Arg, CondBlock (CondBlock))
import Text.Megaparsec (SourcePos)
import PrettyPrint
import Data.List (intercalate)
import MorphUtils (replace)

data MType
  = MInt
  | MDouble
  | MChar
  --  | MTuple [MType]
  --  | MClass Id
  | MFunction [MType] MType
  | MUnknown
  deriving (Show)

instance PrettyPrint MType where
  prettyPrint MInt = "int"
  prettyPrint MDouble = "double"
  prettyPrint MChar = "char"
  prettyPrint MUnknown = "???"
  prettyPrint (MFunction args ret) = "(" <> sig <> ")"
    where
      argSig = intercalate "," $ prettyPrint <$> args
      retSig = prettyPrint ret
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
  deriving (Show)
  -- TODO: Class, type, instance, tuple

indent :: String -> String
indent = ("  " <>) . replace "\n" "\n  "

instance PrettyPrint MExpr where
  prettyPrint (MExprAssignment _ _ lhs rhs) =
    prettyPrint lhs <> " = " <> prettyPrint rhs
  prettyPrint (MExprBlock _ _ body) =
    indent $ intercalate "\n" $ prettyPrint <$> body
  prettyPrint (MExprCall _ _ callee params) =
    prettyPrint callee <> "(" <> intercalate ", " (prettyPrint <$> params) <> ")"
  prettyPrint (MExprIfElse _ _ ifCond elifConds elseBody) =
    ifPrinted <> elifsPrinted <> elsePrinted
    where
      ifPrinted    = "if " <> prettyPrint ifCond
      elifsPrinted = intercalate "\n" (("elif " <>) . prettyPrint <$> elifConds)
      elsePrinted  = "else:\n" <> indent (intercalate "\n" (prettyPrint <$> elseBody))
  prettyPrint (MExprInt _ value)    = show value
  prettyPrint (MExprDouble _ value) = show value
  prettyPrint (MExprChar _ value)   = show value
  prettyPrint (MExprId _ value)     = value
  prettyPrint (MExprDef _ _ args body) =
    "def (" <> argsPrinted <> "):\n" <> indent bodyPrinted
    where
      argsPrinted = intercalate ", " $ prettyPrint <$> args
      bodyPrinted = intercalate "\n" $ prettyPrint <$> body

$(makeLenses ''MExpr)
