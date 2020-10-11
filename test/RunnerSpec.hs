module RunnerSpec where

import Test.Hspec
import Text.Megaparsec
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Maybe
import Debug.Trace

import MontyParser
import MontyRunner
import ParserTypes
import RunnerTypes
import RunnerUtils

exprRepr :: String -> IO String
exprRepr input = do
    eContext <- emptyContext
    val      <- runExceptT $ evalStateT run' eContext
    
    case val of
      Left _    -> undefined
      Right res -> pure $ show res
  
  where
    run' :: Scoper Value
    run' = do
      loadMyLib
      evalP $ head parsed

    parsed :: [PExpr]
    parsed = case parse rootBodyParser "" input of
      Left  err -> trace (errorBundlePretty err) undefined
      Right res -> res

spec :: Spec
spec = do
  describe "Maybe tests" $ do
    it "Folds over maybes" $ do
      exprRepr "Just(3).foldl(1, add)" >>= shouldBe "4"
      exprRepr "None.foldl(1, add)"    >>= shouldBe "1"

    it "Comparison of maybes" $ do
      exprRepr "Just(3) == Just(3)"       >>= shouldBe "True"
      exprRepr "Just(3) == Just(2)"       >>= shouldBe "False"
      exprRepr "None == Just(2)"          >>= shouldBe "False"
      exprRepr "None == None"             >>= shouldBe "True"
      exprRepr "Just(3).compare(Just(4))" >>= shouldBe "LT"
      exprRepr "Just(True).compare(None)" >>= shouldBe "GT"
      exprRepr "Just(1) > Just(2)"        >>= shouldBe "False"
  
    it "Maybe as a Functor" $ do
      exprRepr "Just(2).map((x): x + 10)" >>= shouldBe "Just(12)"
      exprRepr "None.map((x): x + 10)"    >>= shouldBe "None"
      exprRepr "None.mapConst(3)"         >>= shouldBe "None"
      exprRepr "Just(\"hi\").mapConst(3)" >>= shouldBe "Just(3)"

    it "Maybe as an Applicative" $ do
      exprRepr "Just(1).applyr(Just(4))" >>= shouldBe "Just(4)"
      exprRepr "Just(1).applyl(Just(4))" >>= shouldBe "Just(1)"
  
    it "Maybe as a Monad" $ do
      exprRepr "Just(2).bind((x): Just(x + 10))" >>= shouldBe "Just(12)"
      exprRepr "Just(2).bind((x): None)"         >>= shouldBe "None"
      exprRepr "None.bind((x): Just(x))"         >>= shouldBe "None"
      exprRepr "None.bind((x): None)"            >>= shouldBe "None"
      exprRepr "join(Just(Just(1)))"             >>= shouldBe "Just(1)"
      exprRepr "join(None)"                      >>= shouldBe "None"

  describe "Either tests" $ do
    it "Either as a Functor" $ do
      exprRepr "Right(2).map((x): x + 10)" >>= shouldBe "Right(12)"
      exprRepr "Left(6).map((x): x + 1)"   >>= shouldBe "Left(6)"

    it "Either as an Applicative" $ do
      exprRepr "apply(Right((x): x + 1), Right(71))" >>= shouldBe "Right(72)"
      -- TODO: Our pretty print is too stupid to test functions right now
      --       maybe later? who knows? this should be Left((x): x + 1) though
      --exprRepr "apply(Left((x): x + 1), Right(71))"  >>= shouldBe "Right(71)"

    it "Either as a Monad" $ do
      exprRepr "bind(Right(8), (x): Right(x + 1))" >>= shouldBe "Right(9)"
      exprRepr "bind(Left(8), (x): Right(x + 1))"  >>= shouldBe "Left(8)"
      exprRepr "bind(Right(8), (x): Left(x + 1))"  >>= shouldBe "Left(9)"

  describe "General tests" $ do
    it "Until" $ do
      exprRepr "until(-9, ((x): x > 0), ((x): x + 3))" >>= shouldBe "3"
