module PyParser where

import Text.Parsec
import Text.Parsec.String
import Data.Char

-- import foo
-- from foo import bar
-- from foo.bar.it import Baz

data ImportStatement
  = ImportBasic String
  | ImportFrom String String

data Statement = ImportStatement

data InfixOp
  = InfixAdd
  | InfixSub
  | InfixMul
  | InfixDiv
  | InfixMod
  | InfixEq
  | InfixNotEq
  | InfixGreater
  | InfixLess
  | InfixLessEqual
  | InfixGreaterEqual
  | InfixLogicAnd
  | InfixLogicOr
  deriving (Show, Eq)

data Expr
  = ExprId String
  | ExprInt Int
  | ExprInfix Expr InfixOp Expr deriving (Show, Eq)

whitespace :: Parser String
whitespace = many $ char ' '

whitespace1 :: Parser String
whitespace1 = many1 $ char ' '

indent :: Parser Int
indent = length <$> many (string "    " <|> string "\t")

moduleParser :: Parser String
moduleParser = many1 $ alphaNum <|> char '.'

importBasicParser :: Parser ImportStatement
importBasicParser = ImportBasic <$> (string "import" *> whitespace1 *> moduleParser)

importFromParser :: Parser ImportStatement
importFromParser = do
  _     <- string "from"
  place <- moduleParser
  _     <- string "import"
  thing <- moduleParser
  pure $ ImportFrom place thing

-- TODO: See below
--  f(args...)Function call
--  x[index:index]Slicing
--  x[index]Subscription
--  x.attributeAttribute reference
--  **Exponentiation
--  ~xBitwise not
--  +x, -xPositive, negative
--  *, /, %Multiplication, division, remainder
--  +, -Addition, subtraction
--  <<, >>Bitwise shifts
--  &Bitwise AND
--  ^Bitwise XOR
--  |Bitwise OR
--  in, not in, is, is not, <, <=,  >,  >=, <>, !=, ==Comparisons, membership, identity
--  not xBoolean NOT
--  andBoolean AND
--  orBoolean OR
--  lambdaLambda expression

-- Pretty epic
infixOpParser :: Parser InfixOp
infixOpParser = choice $ op <$> arr
  where
    op :: (String, InfixOp) -> Parser InfixOp
    op (s, o) = string s *> pure o
    arr = [
        ("+", InfixAdd),
        ("-", InfixSub),
        ("*", InfixMul),
        ("/", InfixDiv),
        ("%", InfixMod),
        ("==", InfixEq),
        ("!=", InfixNotEq),
        (">", InfixGreater),
        ("<", InfixLess),
        ("<=", InfixLessEqual),
        (">=", InfixGreaterEqual),
        ("&&", InfixLogicAnd),
        ("and", InfixLogicAnd),
        ("||", InfixLogicOr),
        ("or", InfixLogicOr)
      ]

infixParser :: Parser Expr
infixParser = do
  first  <- exprParser'
  _      <- whitespace
  op     <- infixOpParser
  _      <- whitespace
  second <- exprParser
  pure $ ExprInfix first op second

idParser :: Parser Expr
idParser = do
  x  <- char '_' <|> satisfy isAlpha
  xs <- many $ (char '_' <|> alphaNum)
  pure $ ExprId (x:xs)

intParser :: Parser Expr
intParser = ExprInt <$> read <$> many1 digit

parenEater :: Parser Expr -> Parser Expr
parenEater innerParser =
  char '(' *> whitespace *> innerParser <* whitespace <* char ')'

exprParser' :: Parser Expr
exprParser' = try (parenEater exprParser) <|> content
  where content = try idParser <|> intParser

exprParser :: Parser Expr
exprParser = content
  where
    content = try infixParser <|> exprParser'
