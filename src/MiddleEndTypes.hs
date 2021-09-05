{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module MiddleEndTypes where

import Control.Lens
import ParserTypes (Id, Arg, CondBlock (CondBlock))
import Text.Megaparsec (SourcePos)

data MType
  = MInt
  | MDouble
  | MChar
  --  | MTuple [MType]
  --  | MClass Id
  | MFunction [MType] MType
  | MUnknown
  deriving (Show)

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

$(makeLenses ''MExpr)
