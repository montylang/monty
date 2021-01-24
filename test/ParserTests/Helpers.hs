module ParserTests.Helpers where

import Data.Void
import Text.Megaparsec
import Control.Monad.State.Strict

import ParserTypes
import Parser.Utils (eolMany)

myParse :: Parser a -> FilePath -> String -> Either (ParseErrorBundle String Void) a
myParse p = parse (evalStateT p emptyParserState)

testParser :: (Indent -> Parser a) -> String -> Either (ParseErrorBundle String Void) a
testParser p = myParse (p "" <* eolMany <* eof) ""

testParserNoIndent :: Parser a -> String -> Either (ParseErrorBundle String Void) a
testParserNoIndent p = myParse (p <* eolMany <* eof) ""

mkStrExpr = ExprList . fmap (pure . ExprChar)
