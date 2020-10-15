module ParserTypes where

import Data.Void
import Text.Megaparsec hiding (Pos)
import PrettyPrint
import Data.List

type Parser = Parsec Void String

type Indent = String

type Id = String

data CondBlock = CondBlock PExpr [PExpr]
  deriving (Show, Eq)

instance PrettyPrint CondBlock where
  prettyPrint (CondBlock cond body) =
    "(" <> prettyPrint cond <> "):\n" <>
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> body) <> "\n"

data InfixOp
  = InfixAdd  -- Semiring add(a, b): semiring
  | InfixSub  -- Ring
  | InfixMul  -- Semiring
  | InfixDiv
  | InfixMod  -- Semiring
  | InfixEq
  | InfixNe
  | InfixGt
  | InfixLt
  | InfixLe
  | InfixGe
  | InfixLogicAnd -- Logic: lor(a, b): bool
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

data Arg
  = IdArg Id
  | TypedIdArg Id Id
  | PatternArg Id [Arg]
  | SelfArg
  deriving (Show, Eq)

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
  | ExprChar Char
  | ExprIfElse CondBlock [CondBlock] [PExpr]
  | ExprInfix PExpr InfixOp PExpr
  | ExprAssignment Id (PExpr)
  | ExprDef [Arg] [PExpr]
  | ExprCall PExpr [PExpr]
  | ExprReturn PExpr
  | ExprClass Id [Pos TypeCons]
  | ExprList [PExpr]
  | ExprType Id [Pos DefSignature]
  | ExprInstanceOf Id Id [PExpr]
  | ExprBind Id PExpr
  | ExprUnwrap [PExpr]
  | ExprImport [String]
  deriving (Show, Eq)

instance PrettyPrint Expr where
  prettyPrint (ExprId id) = id

  prettyPrint (ExprInt val) = show val

  prettyPrint (ExprChar c) = "'" <> show c <> "'"

  prettyPrint (ExprIfElse ifCB elifCBs elseBody) =
    "if " <> prettyPrint ifCB <>
    (intercalate "" $ (\x -> "elif " <> prettyPrint x) <$> elifCBs) <>
    "else:\n" <>
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> elseBody)

  prettyPrint (ExprInfix lhs op rhs) =
    "(" <> prettyPrint lhs <> prettyPrint op <> prettyPrint rhs <> ")"

  prettyPrint (ExprAssignment name expr) =
    name <> " = " <> prettyPrint expr

  prettyPrint (ExprDef args body) =
    "def(" <> (intercalate ", " $ prettyPrint <$> args) <> "):\n" <>
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> body) <> "\n"

  prettyPrint (ExprCall fun args) =
    prettyPrint fun <> "(" <> (intercalate ", " $ prettyPrint <$> args) <> ")"

  prettyPrint (ExprReturn val) = "return " <> prettyPrint val

  prettyPrint (ExprList values@((Pos _ (ExprChar _)):_)) = show $ tac <$> values
    where
      tac :: Pos Expr -> Char
      tac (Pos _ (ExprChar c)) = c

  prettyPrint (ExprList elements) =
    "[" <> (intercalate ", " $ prettyPrint <$> elements) <> "]"

  prettyPrint (ExprBind name val) = name <> " <- " <> prettyPrint val

  prettyPrint (ExprUnwrap body) =
    "unwrap:\n" <>
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> body) <> "\n"

  -- prettyPrint (ExprInstanceOf Id Id [PExpr]) =
  -- prettyPrint (ExprClass Id [Pos TypeCons]) =
  -- prettyPrint (ExprType Id [Pos DefSignature]) =
  -- prettyPrint (ExprImport [String]) =

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
    getPos :: SourcePos,
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
