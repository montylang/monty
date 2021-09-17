{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module InferenceSpec where

import Test.Hspec

import qualified Data.HashMap.Strict as HM
import Inference
import MiddleEndTypes
import ModuleLoader (montyRunSemantic)
import ParserTypes
import Text.Megaparsec (parse)
import Parser.Root
import Debug.Trace (trace)
import Text.Megaparsec.Error (errorBundlePretty)
import Parser.Semantic (semantic)
import Control.Monad.Except

inferType :: String -> [String]
inferType input = case runTI mexprTypes of
    (Right sigs, _) -> sigs
    (Left err, _)   -> [show err]
  where
    mexprTypes :: TI [String]
    mexprTypes = (show <$>) <$> inferMExprs HM.empty semanticed

    semanticed :: [ExistsMExpr]
    semanticed = forceSemantic <$> parsed

    forceSemantic :: PExpr -> ExistsMExpr
    forceSemantic expr = case runExcept (semantic expr) of
      Right mexpr -> mexpr

    parsed :: [PExpr]
    parsed = case parse rootBodyParser "" input of
      Left  err -> trace (errorBundlePretty err) undefined
      Right res -> res

spec :: Spec
spec = do
  describe "inferMExpr" $ do
    it "Find the type of an int" $ do
      inferType "3" `shouldBe` ["int"]
      inferType "x = 3" `shouldBe` ["int"]

    it "Find type of thunk" $ do
      inferType "a = (): 3" `shouldBe` ["() -> int"]

    it "Find the type of id" $ do
      inferType (unlines [
          "def id(x):",
          "  return x"
        ]) `shouldBe` ["a -> a"]

    it "Find the type of const" $ do
      inferType (unlines [
          "def const(x, y):",
          "  q = x",
          "  return q",
          "const(1, 2)"
        ]) `shouldBe` ["a -> b -> a", "int"]

    it "Find the type of an application" $ do
      inferType (unlines [
          "def id(x):",
          "  return x",
          "",
          "id(3)"
        ]) `shouldBe` ["a -> a", "int"]
