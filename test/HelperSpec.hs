module HelperSpec where

import Test.Hspec

import MorphUtils

spec :: Spec
spec = do
  describe "multiSpan" $ do
    it "Spans multiple conditions" $ do
      multiSpan (< 0) [1, 2, -3, 12] `shouldBe` [[1, 2], [-3, 12]]
      multiSpan (< 0) [1, 2, -3, 12, -1] `shouldBe` [[1, 2], [-3, 12], [-1]]
      multiSpan (< 0) [1, 2, -3, 12, -1, 5] `shouldBe` [[1, 2], [-3, 12], [-1, 5]]
