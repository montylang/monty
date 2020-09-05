import Test.Hspec
import Text.Parsec
import Text.Parsec.String
import Data.Either

import PyParser

testExprParser :: Parser Expr -> String -> Either ParseError Expr
testExprParser parser input = parse (parser <* eof) "" input

main :: IO ()
main = hspec $ do
  describe "Id parser" $ do
    it "Parses abc" $ do
      (testExprParser idParser "abc") `shouldBe` (Right $ ExprId "abc")

    it "Parses _abc" $ do
      (testExprParser idParser "_abc") `shouldBe` (Right $ ExprId "_abc")

    it "Parses _abc123" $ do
      (testExprParser idParser "_abc123") `shouldBe` (Right $ ExprId "_abc123")

    it "Doesn't match vars starting with numbers" $ do
      (testExprParser idParser "1abc") `shouldSatisfy` isLeft

  describe "Int parser" $ do
    it "Parses 123" $ do
      (testExprParser intParser "123") `shouldBe` (Right $ ExprInt 123)

    it "Doesn't parse 123a" $ do
      (testExprParser intParser "123a") `shouldSatisfy` isLeft

  describe "Expr parser" $ do
    it "Simple infix" $ do
      (testExprParser exprParser "a + b") `shouldBe`
        (Right $ ExprInfix (ExprId "a") InfixAdd (ExprId "b"))

    it "Plural infix" $ do
      (testExprParser exprParser "a + b + c") `shouldBe`
        (Right $ ExprInfix (ExprId "a") InfixAdd (ExprInfix (ExprId "b") InfixAdd (ExprId "c")))

    it "Paren eater simpleton" $ do
      (testExprParser exprParser "(a)") `shouldBe`
        (Right $ (ExprId "a"))

    it "Paren eater right associative" $ do
      (testExprParser exprParser "c + (a + b)") `shouldBe`
        (Right $ ExprInfix (ExprId "c") InfixAdd (ExprInfix (ExprId "a") InfixAdd (ExprId "b")))

    it "Paren eater left associative" $ do
      (testExprParser exprParser "(c + a) + b") `shouldBe`
        (Right $ ExprInfix (ExprInfix (ExprId "c") InfixAdd (ExprId "a")) InfixAdd (ExprId "b"))
