module Parser.Literal where

import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Parser.Utils
import ParserTypes

intParser :: Parser Int
intParser = signed sc decimal

charParser :: Parser Char
charParser = char '\'' *> charLiteral <* char '\''

stringParser :: Parser String
stringParser = char '"' >> manyTill charLiteral (char '"')
