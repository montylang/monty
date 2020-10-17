module ParserTests.ArgSpec where

import Test.Hspec
import Text.Megaparsec
import Data.Either

import ParserTypes
import Parser.Arg

import ParserTests.Helpers

spec :: Spec
spec = do
  describe "Id parser" $ do
    it "Parses ids" $ do
      (testParser varIdParser "abc") `shouldBe` (Right $ "abc")
      (testParser varIdParser "_abc") `shouldBe` (Right $ "_abc")
      (testParser varIdParser "_abc123") `shouldBe` (Right $ "_abc123")
      (testParser varIdParser "1abc") `shouldSatisfy` isLeft

  describe "Arg Parser" $ do
    it "Arg parsing" $ do
      (testParser defArgParser "( \t a,  b ,c )") `shouldBe`
        (Right $ [IdArg "a", IdArg "b", IdArg "c"])

    it "Pattern matching" $ do
      (testParser defArgParser "(Just(a), None())") `shouldBe`
        (Right $ [PatternArg "Just" [IdArg "a"], PatternArg "None" []])
