module ParserTypes where

type Indent = String

type Id = String

data CondBlock = CondBlock Expr [Expr]
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

data Expr
  = ExprId Id
  | ExprInt Int
  | ExprString String
  | ExprIfElse CondBlock [CondBlock] [Expr]
  | ExprInfix Expr InfixOp Expr
  | ExprAssignment Id Expr
  | ExprDef [Arg] [Expr]
  | ExprCall Expr [Expr]
  | ExprReturn Expr
  | ExprClass Id [TypeCons]
  | ExprList [Expr]
  | ExprType Id [DefSignature]
  | ExprInstanceOf Id Id [Expr]
  deriving (Show, Eq)

-- DefSignature TypeName FunctionName [Arg]
data DefSignature = DefSignature Id Id [Id]
  deriving (Show, Eq)

data TypeCons = TypeCons Id [Id]
  deriving (Show, Eq)
