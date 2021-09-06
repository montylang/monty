module ParserTypes where

import Data.Void
import Text.Megaparsec hiding (Pos)
import PrettyPrint
import Data.List
import Control.Lens
import Data.Maybe
import Control.Monad

type Parser = Parsec Void String

type Indent = String

type Id = String

data CondBlock a = CondBlock a [a]
  deriving (Show, Eq)

instance PrettyPrint a => PrettyPrint (CondBlock a) where
  prettyPrint (CondBlock cond body) =
    prettyPrint cond <> ":\n" <>
    intercalate "\n" ((\x -> "  " <> prettyPrint x) <$> body) <> "\n"

data CaseBlock a
  = CaseBlock
    { cbpos :: SourcePos,
      cbarg :: Arg,
      cbbody :: [a] }
  deriving (Show)

instance Eq a => Eq (CaseBlock a) where
  (CaseBlock _ xarg xbody) == (CaseBlock _ yarg ybody) =
    xarg == yarg && xbody == ybody

instance PrettyPrint a => PrettyPrint (CaseBlock a) where
  prettyPrint (CaseBlock _ arg body) =
    prettyPrint arg <> ":\n" <>
    intercalate "\n" ((\x -> "  " <> prettyPrint x) <$> body) <> "\n"

data InfixOp
  = InfixAdd
  | InfixSub
  | InfixMul
  | InfixDiv
  | InfixMod
  | InfixEq
  | InfixNe
  | InfixGt
  | InfixLt
  | InfixLe
  | InfixGe
  | InfixLogicAnd
  | InfixLogicOr
  | InfixCons
  | InfixMappend
  deriving (Show, Eq)

instance PrettyPrint InfixOp where
  prettyPrint InfixAdd      = "+"
  prettyPrint InfixSub      = "-"
  prettyPrint InfixMul      = "*"
  prettyPrint InfixDiv      = "/"
  prettyPrint InfixMod      = "%"
  prettyPrint InfixEq       = " ="
  prettyPrint InfixNe       = "!="
  prettyPrint InfixGt       = ">="
  prettyPrint InfixLt       = "<"
  prettyPrint InfixLe       = "<="
  prettyPrint InfixGe       = ">="
  prettyPrint InfixLogicAnd = "and"
  prettyPrint InfixLogicOr  = "or"
  prettyPrint InfixCons     = "|"
  prettyPrint InfixMappend  = "<>"

data PrefixOp
  = PrefixNot
  | PrefixNegate
  deriving (Show, Eq)

instance PrettyPrint PrefixOp where
  prettyPrint PrefixNot = "not"
  prettyPrint PrefixNegate = "-"

data Arg
  = IdArg { _idArgVal :: Id }
  | TypedIdArg
    { _typedIdVal :: Id
    , _typeIdType :: Id }
  | PatternArg
    { _patternName :: Id
    , _patternArgs :: [Arg] }
  | SelfArg
  | IntArg { _intArgVal :: Int }
  | CharArg { _charArgVal :: Char }
  deriving (Show, Eq)

makeLenses ''Arg
makePrisms ''Arg

instance PrettyPrint Arg where
  prettyPrint (IdArg id)             = id
  prettyPrint (TypedIdArg name t)    = name <> ": " <> t
  prettyPrint SelfArg                = "self"
  prettyPrint (PatternArg name args) =
    name <> "(" <> intercalate "," (prettyPrint <$> args) <> ")"

type PExpr = Pos Expr

data Expr
  = ExprId Id
  | ExprInt Int
  | ExprDouble Double
  | ExprChar Char
  | ExprIfElse (CondBlock PExpr) [CondBlock PExpr] (Maybe [PExpr])
  | ExprInfix PExpr InfixOp PExpr
  | ExprPrefixOp PrefixOp PExpr
  | ExprAssignment Arg PExpr
  | ExprDef [Arg] [PExpr]
  | ExprCall PExpr [PExpr]
  | ExprReturn PExpr
  | ExprClass Id [Pos TypeCons]
  | ExprList [PExpr]
  | ExprTuple [PExpr]
  | ExprType Id [Pos DefSignature]
  | ExprInstanceOf Id Id [PExpr]
  | ExprBind Arg PExpr
  | ExprUnwrap [PExpr]
  | ExprImport [String]
  | ExprCase PExpr [CaseBlock PExpr]
  | ExprPrecedence PExpr
  deriving (Show, Eq)

data Tree a = Tree a [Tree a]

data DefSignature = DefSignature {
    getDefSigTypeName :: Id,
    getDefSigFunName :: Id,
    getDefSigArgs :: [Arg],
    getReturnsSelf :: Bool
  }
  deriving (Show, Eq)

data TypeCons = TypeCons {
    getTypeConsName :: Id,
    getTypeConsArgs :: [Id]
  }
  deriving (Show, Eq)

data Pos a = Pos {
    getPosPos :: SourcePos,
    getPosValue :: a
  }

instance PrettyPrint a => PrettyPrint (Pos a) where
  prettyPrint (Pos _ val) = prettyPrint val

instance Show a => Show (Pos a) where
  show (Pos _ val) = show val

instance Eq a => Eq (Pos a) where
  (Pos _ a) == (Pos _ b) = a == b

instance Functor Pos where
  fmap f (Pos pos val) = Pos pos (f val)

instance Applicative Pos where
  (Pos firstPos fun) <*> (Pos secondPos val) =
    Pos (min firstPos secondPos) (fun val)

  pure val = Pos (SourcePos "" (mkPos maxBound) (mkPos maxBound)) val

-- Proof Pos is a monad
instance Monad Pos where
  (Pos pos val) >>= f =
    let (Pos pos2 result) = f val in
      Pos (min pos pos2) result
