module ParserTypes where

import Data.Void
import Text.Megaparsec hiding (Pos)
import PrettyPrint
import Data.List

type Parser = Parsec Void String

type Indent = String

type Id = String

data CondBlock a = CondBlock a [a]
  deriving (Show, Eq)

instance PrettyPrint a => PrettyPrint (CondBlock a) where
  prettyPrint (CondBlock cond body) =
    "(" <> prettyPrint cond <> "):\n" <>
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> body) <> "\n"

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
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> body) <> "\n"

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
  = IdArg Id
  | TypedIdArg Id Id
  | PatternArg Id [Arg]
  | SelfArg
  | IntArg Int
  | CharArg Char
  deriving (Show, Eq)

instance PrettyPrint Arg where
  prettyPrint (IdArg id)             = id
  prettyPrint (TypedIdArg name t)    = name <> ": " <> t
  prettyPrint SelfArg                = "self"
  prettyPrint (PatternArg name args) =
    name <> "(" <> intercalate "," (prettyPrint <$> args) <> ")"

type PExpr = Pos Expr

data RExpr
  = RExprId
      { rpos :: SourcePos
      , rid :: Id }
  | RExprInt
      { rpos :: SourcePos
      , rint :: Int }
  | RExprChar
      { rpos :: SourcePos
      , rchar :: Char }
  | RExprIfElse
      { rpos :: SourcePos
      , rif :: CondBlock RExpr
      , relifs :: [CondBlock RExpr]
      , relseBody :: [RExpr] }
  | RExprInfix
      { rpos :: SourcePos
      , rlhs :: RExpr
      , rinfixOp :: InfixOp
      , rrhs :: RExpr }
  | RExprPrefixOp
      { rpos :: SourcePos
      , rprefixOp :: PrefixOp
      , rexpr :: RExpr }
  | RExprAssignment
      { rpos :: SourcePos
      , rarg :: Arg
      , rval :: RExpr }
  | RExprDef
      { rpos :: SourcePos
      , rargs :: [Arg]
      , rbody :: [RExpr] }
  | RExprCall
      { rpos :: SourcePos
      , rfun :: RExpr
      , rparams :: [RExpr] }
  | RExprReturn
      { rpos :: SourcePos
      , rretVal :: RExpr }
  | RExprClass
      { rpos :: SourcePos
      , rcname :: Id
      , rtypecons :: [Pos TypeCons] }
  | RExprList
      { rpos :: SourcePos
      , relements :: [RExpr] }
  | RExprTuple
      { rpos :: SourcePos
      , relements :: [RExpr] }
  | RExprType
      { rpos :: SourcePos
      , rtname :: Id
      , rdefSigs :: [Pos DefSignature] }
  | RExprInstanceOf
      { rpos :: SourcePos
      , rcname :: Id
      , rtname :: Id
      , rbody :: [RExpr] }
  | RExprImport
      { rpos :: SourcePos
      , rpath :: [String] }
  | RExprCase
      { rpos :: SourcePos
      , rinput :: RExpr
      , rcaseBlocks :: [CaseBlock RExpr] }

data Expr
  = ExprId Id
  | ExprInt Int
  | ExprChar Char
  | ExprIfElse (CondBlock PExpr) [CondBlock PExpr] [PExpr]
  | ExprInfix PExpr InfixOp PExpr
  | ExprPrefixOp PrefixOp PExpr
  | ExprAssignment Arg (PExpr)
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

instance PrettyPrint RExpr where
  prettyPrint (RExprId _ id) = id

  prettyPrint (RExprInt _ val) = show val

  prettyPrint (RExprChar _ c) = "'" <> show c <> "'"

  prettyPrint (RExprIfElse _ ifCB elifCBs elseBody) =
    "if " <> prettyPrint ifCB <>
    (intercalate "" $ (\x -> "elif " <> prettyPrint x) <$> elifCBs) <>
    "else:\n" <>
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> elseBody)

  prettyPrint (RExprInfix _ lhs op rhs) =
    "(" <> prettyPrint lhs <> prettyPrint op <> prettyPrint rhs <> ")"

  prettyPrint (RExprAssignment _ dest expr) =
    prettyPrint dest <> " = " <> prettyPrint expr

  prettyPrint (RExprDef _ args body) =
    "def(" <> (intercalate ", " $ prettyPrint <$> args) <> "):\n" <>
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> body) <> "\n"

  prettyPrint (RExprCall _ fun args) =
    prettyPrint fun <> "(" <> (intercalate ", " $ prettyPrint <$> args) <> ")"

  prettyPrint (RExprReturn _ val) = "return " <> prettyPrint val

  prettyPrint (RExprList _ values@((RExprChar _ _):_)) = show $ tac <$> values
    where
      tac :: RExpr -> Char
      tac (RExprChar _ c) = c

  prettyPrint (RExprList _ elements) =
    "[" <> (intercalate ", " $ prettyPrint <$> elements) <> "]"

  prettyPrint (RExprCase _ input bodies) =
    "case " <> prettyPrint input <> ":\n" <>
    (intercalate "\n" $ (\x -> "  " <> prettyPrint x) <$> bodies) <> "\n"

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
