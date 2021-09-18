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
import Parser.TypeParser (parseSig)

inferType :: TypeEnv -> String -> [String]
inferType env input = case runTI mexprTypes of
    (Right sigs, _) -> sigs
    (Left err, _)   -> [show err]
  where
    mexprTypes :: TI [String]
    mexprTypes = do
      semanticed' <- semanticed
      (show <$>) <$> inferMExprs env semanticed'

    semanticed :: TI [ExistsMExpr]
    semanticed = traverse forceSemantic =<< parsed

    forceSemantic :: PExpr -> TI ExistsMExpr
    forceSemantic expr = case runExcept (semantic expr) of
      Right mexpr -> pure mexpr
      Left err    -> throwError (show err)

    parsed :: TI [PExpr]
    parsed = case parse rootBodyParser "" input of
      Left  err -> throwError $ errorBundlePretty err
      Right res -> pure res

inferType' = inferType emptyTypeEnv

parseTypeOrDie :: String -> MType
parseTypeOrDie s = case parse parseSig "" s of
  Left err -> trace (errorBundlePretty err) undefined
  Right res -> res

parseSchemeOrDie :: String -> Scheme
parseSchemeOrDie = generalize emptyTypeEnv . parseTypeOrDie

stdEnv :: TypeEnv
stdEnv = TypeEnv $ HM.fromList [
    ("#add",      parseSchemeOrDie "Int -> Int -> Int"),
    ("#subtract", parseSchemeOrDie "Int -> Int -> Int"),
    ("#multiply", parseSchemeOrDie "Int -> Int -> Int"),
    ("#or",       parseSchemeOrDie "Bool -> Bool -> Bool"),
    ("#equals",   parseSchemeOrDie "a -> a -> Bool"),
    ("const",     parseSchemeOrDie "a -> b -> a"),
    -- fix cannot inherently be implicitly typed unfortunately
    ("fix",       parseSchemeOrDie "(a -> a) -> a")
  ]

spec :: Spec
spec = do
  describe "inferMExpr" $ do
    it "Find the type of an int" $ do
      inferType' "3" `shouldBe` ["Int"]
      inferType' "x = 3" `shouldBe` ["Int"]

    it "Find type of thunk" $ do
      inferType' "a = (): 3" `shouldBe` ["() -> Int"]

    it "Find the type of id" $ do
      inferType' (unlines [
          "def id(x):",
          "  return x"
        ]) `shouldBe` ["a -> a"]

    it "Find the type of const" $ do
      inferType' (unlines [
          "def const(x, y):",
          "  q = x",
          "  return q",
          "const(1, 2)"
        ]) `shouldBe` ["a -> b -> a", "Int"]

    it "Find the type of an application" $ do
      inferType' (unlines [
          "def id(x):",
          "  return x",
          "",
          "id(3)"
        ]) `shouldBe` ["a -> a", "Int"]

    it "Infer an add expression" $ do
      inferType stdEnv (unlines [
          "def add1(x):",
          "  return x + 1"
        ]) `shouldBe` ["Int -> Int"]

    it "Infer a conditional type" $ do
      inferType stdEnv (unlines [
          "def (x):",
          "  return if True:",
          "    3",
          "  else:",
          "    x"
        ]) `shouldBe` ["Int -> Int"]

      inferType stdEnv (unlines [
          "def (x):",
          "  return if True:",
          "    x",
          "  else:",
          "    3"
        ]) `shouldBe` ["Int -> Int"]

      inferType stdEnv (unlines [
          "def (x):",
          "  return if True:",
          "    (y): const(x, True or y)",
          "  else:",
          "    (y): const(3, y)"
        ]) `shouldBe` ["Int -> Bool -> Int"]

      inferType stdEnv (unlines [
          "def (x):",
          "  return if True:",
          "    (y): const(3, y)",
          "  else:",
          "    (y): const(x, True or y)"
        ]) `shouldBe` ["Int -> Bool -> Int"]

    it "Infer type of a recursive function" $ do
      inferType stdEnv (unlines [
          "def factorialP(f, n):",
          "  if n == 0:",
          "    return 1",
          "  else:",
          "    return n * f(n - 1)",
          "",
          "factorial = fix(factorialP)"
        ]) `shouldBe` ["(Int -> Int) -> Int -> Int", "Int -> Int"]

      -- TODO: don't require fix
      -- inferType stdEnv (unlines [
      --     "def factorial(n):",
      --     "  if n == 0:",
      --     "    return 1",
      --     "  else:",
      --     "    return n * factorial(n - 1)"
      --   ]) `shouldBe` ["Int -> Int"]

