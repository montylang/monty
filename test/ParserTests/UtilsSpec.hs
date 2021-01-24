module ParserTests.UtilsSpec where

import Test.Hspec
import Text.Megaparsec
import Parser.Utils (singleEol)
import ParserTests.Helpers

spec :: Spec
spec = do
  describe "SingleEol parser" $ do
    it "Parses EOLs" $ do
      myParse (singleEol <* eof) "" "  \n" `shouldBe` (Right ())
      myParse (singleEol <* eof) "" "  # \n" `shouldBe` (Right ())
      myParse (singleEol <* eof) "" "  #  sntaoehusnthoea \n" `shouldBe` (Right ())
