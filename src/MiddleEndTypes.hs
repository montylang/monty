{-# LANGUAGE TypeApplications, DataKinds, KindSignatures, RankNTypes,
             StandaloneKindSignatures, PolyKinds, TypeSynonymInstances,
             FlexibleInstances #-}
module MiddleEndTypes where

import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
import Control.Lens
import ParserTypes (Id, Arg, CondBlock (CondBlock))
import Text.Megaparsec (SourcePos)
import Data.List (intercalate)
import MorphUtils
import Control.Monad.State (State)

type ExistsMExpr = Exists @MExprType MExpr

instance Show ExistsMExpr where
  show (Exists mexpr) = show mexpr

data MType
  = MVar { _typeVar :: String }
  | MInt
  | MFun { _inputTypes :: [MType], _outputType :: MType }
  deriving (Eq, Ord)

instance Show MType where
  show (MVar name) = name
  show MInt = "int"
  show (MFun args ret) = "(" <> sig <> ")"
    where
      argSig = intercalate "," $ show <$> args
      retSig = show ret
      sig = argSig <> " -> " <> retSig

data MExprType
  = MExprIdType
  | MExprAssignmentType
  | MExprBlockType
  | MExprCallType
  | MExprIfElseType
  | MExprIntType
  | MExprDoubleType
  | MExprCharType
  | MExprDefType

data MExpr (a :: MExprType) where
  MExprId ::
    { _idMpos :: SourcePos
    , _id :: Id } -> MExpr MExprIdType
  MExprAssignment ::
    { _assignmentMpos :: SourcePos
    , _lhs :: Arg
    , _rhs :: ExistsMExpr } -> MExpr MExprAssignmentType
  MExprBlock ::
    { _blockMpos :: SourcePos
    , _blockBody :: [ExistsMExpr] } -> MExpr MExprBlockType
  MExprCall ::
    { _callMpos :: SourcePos
    , _callee :: ExistsMExpr
    , _params :: [ExistsMExpr] } -> MExpr MExprCallType
  MExprIfElse ::
    { _ifelseMpos :: SourcePos
    , _ifCond :: CondBlock ExistsMExpr
    , _elifConds :: [CondBlock ExistsMExpr]
    , _elseBody :: [ExistsMExpr] } -> MExpr MExprIfElseType
  MExprInt ::
    { _intMpos :: SourcePos
    , _intValue :: Int } -> MExpr MExprIntType
  MExprDouble ::
    { _doubleMpos :: SourcePos
    , _doubleValue :: Double } -> MExpr MExprDoubleType
  MExprChar ::
    { _charMpos :: SourcePos
    , _charValue :: Char } -> MExpr MExprCharType
  MExprDef :: -- TODO: Maybe store available type sigs elsewhere
    { _defMpos :: SourcePos
    , _args :: [Arg]
    , _defBody :: [ExistsMExpr]
    } -> MExpr MExprDefType -- TODO: Maybe store available type sigs elsewhere
  -- TODO: Class, type, instance, tuple

mpos :: Lens' (MExpr a) SourcePos
mpos f expr@MExprId { _idMpos } =
  (\mpos' -> expr {_idMpos = mpos'}) <$> f _idMpos
mpos f expr@MExprAssignment { _assignmentMpos } =
  (\mpos' -> expr {_assignmentMpos = mpos'}) <$> f _assignmentMpos
mpos f expr@MExprBlock { _blockMpos } =
  (\mpos' -> expr {_blockMpos = mpos'}) <$> f _blockMpos
mpos f expr@MExprCall{ _callMpos } =
  (\mpos' -> expr {_callMpos = mpos'}) <$> f _callMpos
mpos f expr@MExprIfElse{ _ifelseMpos } =
  (\mpos' -> expr {_ifelseMpos = mpos'}) <$> f _ifelseMpos
mpos f expr@MExprInt{ _intMpos } =
  (\mpos' -> expr {_intMpos = mpos'}) <$> f _intMpos
mpos f expr@MExprDouble{ _doubleMpos } =
  (\mpos' -> expr {_doubleMpos = mpos'}) <$> f _doubleMpos
mpos f expr@MExprChar{ _charMpos } =
  (\mpos' -> expr {_charMpos = mpos'}) <$> f _charMpos
mpos f expr@MExprDef{ _defMpos } =
  (\mpos' -> expr {_defMpos = mpos'}) <$> f _defMpos

indent :: String -> String
indent = ("  " <>) . replace "\n" "\n  "

instance Show (MExpr a) where
  show (MExprAssignment _ lhs rhs) = [i|#{lhs} = #{rhs}|]
  show (MExprBlock _ body) =
    indent $ intercalate "\n" $ show <$> body
  show (MExprCall _ callee params) =
    let args = intercalate ", " (show <$> params) in
      [i|#{callee}(#{args})|]
  show (MExprIfElse _ ifCond elifConds elseBody) =
    ifPrinted <> elifsPrinted <> elsePrinted
    where
      ifPrinted    = [i|if #{ifCond}|]
      elifsPrinted = intercalate "\n" (("elif " <>) . show <$> elifConds)
      elsePrinted  = "else:\n" <> indent (intercalate "\n" (show <$> elseBody))
  show (MExprInt _ value)    = show value
  show (MExprDouble _ value) = show value
  show (MExprChar _ value)   = show value
  show (MExprId _ value)     = value
  show (MExprDef _ args body) =
    [i|def (#{argsPrinted}):\n|] <> indent bodyPrinted
    where
      argsPrinted = intercalate ", " $ show <$> args
      bodyPrinted = intercalate "\n" $ show <$> body
