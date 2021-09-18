module ParserTests.TypeParserSpec where

import Test.Hspec
import Text.Megaparsec
import Data.Either

import ParserTypes
import MiddleEndTypes
import Parser.TypeParser
import Parser.Utils (rword)

import ParserTests.Helpers

spec :: Spec
spec = do
  describe "Simple types" $ do
    it "Parses vars" $ do
      parse (parseSig <* eof) "" "a" `shouldBe` Right (MVar "a")

    it "Parses int" $ do
      parse (parseSig <* eof) "" "Int" `shouldBe` Right MInt

    it "Parses bool" $ do
      parse (parseSig <* eof) "" "Bool" `shouldBe` Right MBool

  describe "Function types" $ do
    it "Parses a -> b" $ do
      parse (parseSig <* eof) "" "a -> b" `shouldBe` Right (MFun (MVar "a") (MVar "b"))

  describe "Disambiguates with parens" $ do
    it "Parses (a -> b)" $ do
      parse (parseSig <* eof) "" "(a -> b)" `shouldBe` Right (MFun (MVar "a") (MVar "b"))

    it "Parses a -> (  b )" $ do
      parse (parseSig <* eof) "" "(a -> b)" `shouldBe` Right (MFun (MVar "a") (MVar "b"))

    it "Parses (b -> c) -> d" $ do
      parse (parseSig <* eof) "" "(c -> b) -> d" `shouldBe` Right
        (MFun (MFun (MVar "c") (MVar "b")) (MVar "d"))

    it "Parses a -> (b -> c) -> d" $ do
      parse (parseSig <* eof) "" "a -> (b -> c) -> d" `shouldBe` Right
        (MFun (MVar "a") (MFun (MFun (MVar "b") (MVar "c")) (MVar "d")))
