module ParserTypes where

import Data.Void
import Text.Megaparsec hiding (Pos)
import Data.List
import Control.Lens
import Data.Maybe
import Control.Monad

type Parser = Parsec Void String

type Indent = String

type Id = String

data CondBlock a = CondBlock a [a]
  deriving (Eq)

instance Show a => Show (CondBlock a) where
  show (CondBlock cond body) =
    show cond <> ":\n" <>
    intercalate "\n" ((\x -> "  " <> show x) <$> body) <> "\n"

data CaseBlock a
  = CaseBlock
    { cbpos :: SourcePos,
      cbarg :: Arg,
      cbbody :: [a] }

instance Eq a => Eq (CaseBlock a) where
  (CaseBlock _ xarg xbody) == (CaseBlock _ yarg ybody) =
    xarg == yarg && xbody == ybody

instance Show a => Show (CaseBlock a) where
  show (CaseBlock _ arg body) =
    show arg <> ":\n" <>
    intercalate "\n" ((\x -> "  " <> show x) <$> body) <> "\n"

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
  deriving (Eq)

instance Show InfixOp where
  show InfixAdd      = "+"
  show InfixSub      = "-"
  show InfixMul      = "*"
  show InfixDiv      = "/"
  show InfixMod      = "%"
  show InfixEq       = " ="
  show InfixNe       = "!="
  show InfixGt       = ">="
  show InfixLt       = "<"
  show InfixLe       = "<="
  show InfixGe       = ">="
  show InfixLogicAnd = "and"
  show InfixLogicOr  = "or"
  show InfixCons     = "|"
  show InfixMappend  = "<>"

data PrefixOp
  = PrefixNot
  | PrefixNegate
  deriving (Eq)

instance Show PrefixOp where
  show PrefixNot = "not"
  show PrefixNegate = "-"

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
  deriving (Eq)

makeLenses ''Arg
makePrisms ''Arg

instance Show Arg where
  show (IdArg id)             = id
  show (TypedIdArg name t)    = name <> ": " <> t
  show SelfArg                = "self"
  show (PatternArg name args) =
    name <> "(" <> intercalate "," (show <$> args) <> ")"

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
