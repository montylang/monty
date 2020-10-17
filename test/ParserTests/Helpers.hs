module ParserTests.Helpers where

import Data.Void
import Text.Megaparsec

import ParserTypes
import Parser.Utils (singleEol)

testParser :: (Indent -> Parser a) -> String -> Either (ParseErrorBundle String Void) a
testParser p input = parse (p "" <* (try $ many $ try singleEol) <* eof) "" input

mkStrExpr = ExprList . fmap (pure . ExprChar)
