module Parser.Literal where

import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Parser.Utils
import ParserTypes

intParser :: Parser Int
intParser = try $ signed sc decimal

doubleParser :: Parser Double
doubleParser = try $ signed sc float

charParser :: Parser Char
charParser = char '\'' *> charLiteral <* char '\''

stringParser :: Parser String
stringParser = char '"' >> manyTill charLiteral (char '"')
