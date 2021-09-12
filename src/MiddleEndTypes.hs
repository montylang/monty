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
  , _sigs :: [FuncSig]
  , _mexpr :: Maybe ExistsMExpr
  }

instance Show FuncDef where
  show _ = "funcdef"

type FuncTable = HM.HashMap Int FuncDef

showFuncTable :: FuncTable -> String
showFuncTable table = intercalate "\n" $ uncurry showEntry <$> HM.toList table
  where
    showEntry :: Int -> FuncDef -> String
    showEntry id def =
      show id <>
      showName def <>
      "[" <> show (_argCount def) <> "]\n" <>
      intercalate "\n" (("  - " <>) . show <$> _sigs def)

    showName :: FuncDef -> String
    showName def = case _name def of
      Just name -> "(" <> name <> ")"
      _         -> "<anon>"

showIdTable :: HM.HashMap String ExistsMExpr -> String
showIdTable table = intercalate "\n" $ uncurry showEntry <$> HM.toList table
  where
    showEntry :: String -> ExistsMExpr -> String
    showEntry id expr = [i|#{id} = #{expr}|]

data Env = Env
  { _funcTable :: FuncTable
  , _latestFuncId :: Int
  , _idTable :: HM.HashMap String (Exists @MExprType MExpr)
  }

instance Show Env where
  show Env { _funcTable, _idTable } =
    "Environment:\n" <>
    "Function table:\n" <> showFuncTable _funcTable <>
    "Constant table:\n" <> showIdTable _idTable

emptyFuncTable :: FuncTable
emptyFuncTable = HM.empty

emptyEnv = Env emptyFuncTable 0 HM.empty

type ExistsMExpr = Exists @MExprType MExpr

instance Show ExistsMExpr where
  show (Exists mexpr) = show mexpr

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
    , _assignmentMtype :: MType
    , _lhs :: Arg
    , _rhs :: ExistsMExpr } -> MExpr MExprAssignmentType
  MExprBlock ::
    { _blockMpos :: SourcePos
    , _blockMtype :: MType
    , _blockBody :: [ExistsMExpr] } -> MExpr MExprBlockType
  MExprCall ::
    { _callMpos :: SourcePos
    , _callMtype :: MType
    , _callee :: ExistsMExpr
    , _params :: [ExistsMExpr] } -> MExpr MExprCallType
  MExprIfElse ::
    { _ifelseMpos :: SourcePos
    , _ifelseMtype :: MType
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
    , _funcId :: Maybe Int
    , _args :: [Arg]
    , _defBody :: [ExistsMExpr]
    } -> MExpr MExprDefType -- TODO: Maybe store available type sigs elsewhere
  -- TODO: Class, type, instance, tuple

funcId :: Lens' (MExpr MExprDefType) (Maybe Int)
funcId f expr@MExprDef {_funcId} =
  (\funcId' -> expr {_funcId = funcId'}) <$> f _funcId

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
  show (MExprAssignment _ _ lhs rhs) = [i|#{lhs} = #{rhs}|]
  show (MExprBlock _ _ body) =
    indent $ intercalate "\n" $ show <$> body
  show (MExprCall _ _ callee params) =
    let args = intercalate ", " (show <$> params) in
      [i|#{callee}(#{args})|]
  show (MExprIfElse _ _ ifCond elifConds elseBody) =
    ifPrinted <> elifsPrinted <> elsePrinted
    where
      ifPrinted    = [i|if #{ifCond}|]
      elifsPrinted = intercalate "\n" (("elif " <>) . show <$> elifConds)
      elsePrinted  = "else:\n" <> indent (intercalate "\n" (show <$> elseBody))
  show (MExprInt _ value)    = show value
  show (MExprDouble _ value) = show value
  show (MExprChar _ value)   = show value
  show (MExprId _ value)     = value
  show (MExprDef _ _ args body) =
    [i|def (#{argsPrinted}):\n|] <> indent bodyPrinted
    where
      argsPrinted = intercalate ", " $ show <$> args
      bodyPrinted = intercalate "\n" $ show <$> body

-- $(makeLenses ''MExpr)

$(makeLenses ''FuncSig)
$(makeLenses ''FuncDef)
$(makeLenses ''Env)
