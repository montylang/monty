{-# LANGUAGE TemplateHaskell, KindSignatures, DataKinds, RankNTypes #-}
module MiddleEndTypes where

import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
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

data FuncDef a = FuncDef
  { _name :: Maybe String
  , _sourcePos :: Maybe SourcePos
  , _argCount :: Int
  , _sigs :: [FuncSig]
  , _mexpr :: Maybe (MExpr a)
  }

instance Show (FuncDef a) where
  show _ = "funcdef"

type FuncTable a = HM.HashMap Int (FuncDef a)

showFuncTable :: FuncTable a -> String
showFuncTable table = intercalate "\n" $ uncurry showEntry <$> HM.toList table
  where
    showEntry :: Int -> FuncDef a -> String
    showEntry id def =
      show id <>
      showName def <>
      "[" <> show (_argCount def) <> "]\n" <>
      intercalate "\n" (("  - " <>) . show <$> _sigs def)

    showName :: FuncDef a -> String
    showName def = case _name def of
      Just name -> "(" <> name <> ")"
      _         -> "<anon>"

showIdTable :: HM.HashMap String (MExpr a) -> String
showIdTable table = intercalate "\n" $ uncurry showEntry <$> HM.toList table
  where
    showEntry :: String -> MExpr a -> String
    showEntry id expr = [i|#{id} = #{expr}|]

data Env a = Env
  { _funcTable :: FuncTable a
  , _latestFuncId :: Int
  , _idTable :: HM.HashMap String (MExpr a)
  }

instance Show (Env a) where
  show Env { _funcTable, _idTable } =
    "Environment:\n" <>
    "Function table:\n" <> showFuncTable _funcTable <>
    "Constant table:\n" <> showIdTable _idTable

emptyFuncTable :: FuncTable a
emptyFuncTable = HM.empty

emptyEnv = Env emptyFuncTable 0 HM.empty

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

-- data YeeType = Id
--              | Assignment

-- data MExprTest (a :: YeeType) where
--   PId :: String -> MExprTest 'Id
--   PAssignment :: String -> MExprTest 'Assignment

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
    , _rhs :: forall a. MExpr a } -> MExpr MExprAssignmentType
  MExprBlock ::
    { _blockMpos :: SourcePos
    , _blockMtype :: MType
    , _blockBody :: forall a. [MExpr a] } -> MExpr MExprBlockType
  MExprCall ::
    { _callMpos :: SourcePos
    , _callMtype :: MType
    , _callee :: forall (b :: MExprType). MExpr b
    , _params :: forall (c :: MExprType). [MExpr c] } -> MExpr MExprCallType
  MExprIfElse ::
    { _ifElseMpos :: SourcePos
    , _ifElseMtype :: MType
    , _ifCond :: forall a. CondBlock (MExpr a)
    , _elifConds :: forall a. [CondBlock (MExpr a)]
    , _elseBody :: forall a. [MExpr a] } -> MExpr MExprIfElseType
  MExprInt ::
    { _intMpos :: SourcePos
    , _ivalue :: Int } -> MExpr MExprIntType
  MExprDouble ::
    { _doubleMpos :: SourcePos
    , _dvalue :: Double } -> MExpr MExprDoubleType
  MExprChar ::
    { _charMpos :: SourcePos
    , _cvalue :: Char } -> MExpr MExprCharType
  MExprDef :: -- TODO: Maybe store available type sigs elsewhere
    { _defMpos :: SourcePos
    , _funcId :: Maybe Int
    , _args :: [Arg]
    , _defBody :: forall a. [MExpr a]
    } -> MExpr MExprDefType
  -- TODO: Class, type, instance, tuple

indent :: String -> String
indent = ("  " <>) . replace "\n" "\n  "

instance Show (MExpr a) where
  show (MExprAssignment _ _ lhs rhs) =
    [i|#{lhs} = #{rhs}|]
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

mpos :: Lens' (MExpr a) SourcePos
mpos f id@MExprId { _idMpos } =
  (\mpos' -> id { _idMpos = mpos' }) <$> f _idMpos

$(makeLenses ''FuncSig)
$(makeLenses ''FuncDef)
$(makeLenses ''Env)
