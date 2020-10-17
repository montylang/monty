module ParserTests.UtilsSpec where

import Test.Hspec
import Text.Megaparsec
import Parser.Utils (singleEol)

spec :: Spec
spec = do
  describe "SingleEol parser" $ do
    it "Parses EOLs" $ do
      parse (singleEol <* eof) "" "  \n" `shouldBe` (Right ())
      parse (singleEol <* eof) "" "  # \n" `shouldBe` (Right ())
      parse (singleEol <* eof) "" "  #  sntaoehusnthoea \n" `shouldBe` (Right ())
