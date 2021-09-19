{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module InferenceSpec where

import MyPrelude

import Test.Hspec

import qualified Data.HashMap.Strict as HM
import Inference
import InferenceTypes
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
import System.Exit (die)

hasTypes :: [String] -> [(String, String)] -> IO ()
hasTypes lines expected = do
  case runTI wubble of
    (Right _, tiState) -> do
      let defs = tiState ^. globalDefs
      traverse_ (f defs) expected
    (Left err, _) -> expectationFailure err
  where
    f :: HM.HashMap String MType -> (String, String) -> IO ()
    f defs (expectedName, expectedType) = do
      -- TODO: Compare MType instead of strings
      show <$> HM.lookup expectedName defs `shouldBe` Just expectedType

    wubble :: TI () 
    wubble = do
      globalDefs .= builtinTypes
      parseProg (unlines lines) >>= inferTopLevelDefs

inferType :: TypeEnv -> String -> [String]
inferType env input = case runTI mexprTypes of
    (Right sigs, _) -> sigs
    (Left err, _)   -> [show err]
  where
    mexprTypes :: TI [String]
    mexprTypes = do
      semanticed' <- parseProg input
      (show <$>) <$> inferMExprs env semanticed'

parseProg :: String -> TI [ExistsMExpr]
parseProg input = traverse forceSemantic =<< parsed
  where
    forceSemantic :: PExpr -> TI ExistsMExpr
    forceSemantic expr = case runExcept (semantic expr) of
      Right mexpr -> pure mexpr
      Left err    -> throwError (show err)

    parsed :: TI [PExpr]
    parsed = case parse rootBodyParser "" input of
      Left  err -> throwError $ errorBundlePretty err
      Right res -> pure res
    

inferType' = inferType emptyTypeEnv

stdEnv :: TypeEnv
stdEnv = TypeEnv $ HM.map (generalize emptyTypeEnv) builtinTypes

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
      ["def const(x, y):",
       "  q = x",
       "  return q",
       "z = const(1, 2)"]
      `hasTypes`
      [("const", "a -> b -> a"),
       ("z",     "Int")]

    it "Find the type of an application" $ do
      ["def id(x):",
       "  return x",
       "",
       "z = id(3)"]
      `hasTypes`
      [("id", "a -> a"),
       ("z", "Int")]

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

    it "Infer type of return type of a function parameter:" $ do
      inferType stdEnv (unlines [
          "def (x):",
          "  return True or x(1)"
        ]) `shouldBe` ["(Int -> Bool) -> Bool"]

      inferType stdEnv (unlines [
          "def (x):",
          "  return if x:",
          "    True",
          "  else:",
          "    False"
        ]) `shouldBe` ["Bool -> Bool"]

      inferType stdEnv (unlines [
          "def (x):",
          "  return if x(1):",
          "    True",
          "  else:",
          "    False"
        ]) `shouldBe` ["(Int -> Bool) -> Bool"]

    it "Infer type of a recursive function" $ do
      hasTypes
        ["def factorialP(f, n):",
         "  if n == 0:",
         "    return 1",
         "  else:",
         "    return n * f(n - 1)",
         "",
         "factorial = fix(factorialP)"]
        [("factorialP", "(Int -> Int) -> Int -> Int"),
         ("factorial", "Int -> Int")]

      hasTypes
        ["def factorial(n):",
         "  if n == 0:",
         "    return 1",
         "  else:",
         "    return n * factorial(n - 1)"]
        [("factorial", "Int -> Int")]

      hasTypes
        ["f = f"]
        [("f", "a")]

      hasTypes
        ["def f():",
         "  return f()"]
        [("f", "() -> a")]

      hasTypes
        ["def f(x):",
         "  return f(x)"]
        [("f", "a -> b")]

    it "Infer type of mutually recursive functions" $ do
      hasTypes
        ["def isEven(x):",
         "  if x == 0:",
         "    return True",
         "  return not isOdd(x - 1)",
         "def isOdd(x):",
         "  if x == 1:",
         "    return True",
         "  return not isEven(x - 1)"]
        [("isEven", "Int -> Bool"),
         ("isOdd", "Int -> Bool")]

      hasTypes
        ["f = g",
         "g = f"]
        [("f", "a"),
         ("g", "a")]

      hasTypes
        ["def f(x):",
         "  return g(x)",
         "def g(x):",
         "  return f(x)"]
        [("f", "a -> b"),
         ("g", "a -> b")]

      hasTypes
        ["def f():",
         "  return g()",
         "def g():",
         "  return f()"]
        [("f", "() -> a"),
         ("g", "() -> a")]

    it "Infer type of a recursive function with function param" $ do
      hasTypes
        ["def until(initial, predicate, func):",
         "  return if predicate(initial):",
         "    initial",
         "  else:",
         "    until(func(initial), predicate, func)"]
        [("until", "a -> (a -> Bool) -> (a -> a) -> a")]
