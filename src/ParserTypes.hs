module ParserTypes where

import Text.Parsec.Pos

type Indent = String

type Id = String

data CondBlock = CondBlock PExpr [PExpr]
  deriving (Show, Eq)

data InfixOp
  = InfixAdd  -- Semiring add(a, b): semiring
  | InfixSub  -- Ring
  | InfixMul  -- Semiring
  | InfixDiv
  | InfixMod  -- Semiring
  | InfixEq       -- Equal equal(a, b): bool
  | InfixNotEq    -- Equal
  | InfixGreater  -- Ord : ord(a, b) == GT
  | InfixLess
  | InfixLessEqual
  | InfixGreaterEqual
  | InfixLogicAnd -- Logic: lor(a, b): bool
  | InfixLogicOr
  | InfixCons
  deriving (Show, Eq)

data Arg
  = IdArg Id
  | PatternArg Id [Arg]
  deriving (Show, Eq)

type PExpr = Pos Expr

data Expr
  = ExprId Id
  | ExprInt Int
  | ExprString String
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
  deriving (Show, Eq)

-- DefSignature TypeName FunctionName [Arg]
data DefSignature = DefSignature Id Id [Id]
  deriving (Show, Eq)

data TypeCons = TypeCons Id [Id]
  deriving (Show, Eq)

data Pos a = Pos {
    getPos :: SourcePos,
    getPosValue :: a
  }
  deriving (Show, Eq)

instance Functor Pos where
  fmap f (Pos pos val) = Pos pos (f val)

instance Applicative Pos where
  (Pos firstPos fun) <*> (Pos secondPos val) =
    Pos (min firstPos secondPos) (fun val)

  pure val = Pos (newPos "" maxBound maxBound) val

-- Proof Pos is a monad
instance Monad Pos where
  (Pos pos val) >>= f =
    let (Pos pos2 result) = f val in
      Pos (min pos pos2) result
 
