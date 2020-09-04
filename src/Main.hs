module Main where

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

data InfixOp = InfixAdd | InfixSub | InfixMul | InfixDiv | InfixMod deriving (Show)

data Expr
  = ExprId String
  | ExprInfix Expr InfixOp Expr deriving (Show)

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
        ("%", InfixMod)
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

exprParser' :: Parser Expr
exprParser' = idParser

exprParser :: Parser Expr
exprParser = exprParser' <|> infixParser

p :: String -> String
p input = show $ parse (exprParser <* eof) "" input

main :: IO ()
main = do
  putStrLn "hello world"
