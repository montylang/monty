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

runWithContext :: Context -> String -> IO String
runWithContext context input = do
    val <- runExceptT $ evalStateT run' context
    
    case val of
      Left _    -> undefined
      Right res -> pure $ show res
  where
    run' :: Scoper Value
    run' = evalP $ head parsed

    parsed :: [PExpr]
    parsed = case parse rootBodyParser "" input of
      Left  err -> trace (errorBundlePretty err) undefined
      Right res -> res

loadPrelude :: IO Context
loadPrelude = do
    eContext <- emptyContext
    val      <- runExceptT $ evalStateT load eContext
    
    case val of
      Left _    -> undefined
      Right res -> pure res
  where
    load :: Scoper Context
    load = loadMyLib *> get

spec :: Spec
spec = do
  exprRepr <- runWithContext <$> runIO loadPrelude

  describe "Arithmetic tests" $ do
    it "Should add integers" $ do
      exprRepr "3 + 4 + 2" >>= shouldBe "9"
      exprRepr "1 + 1 + 1" >>= shouldBe "3"
      exprRepr "1 + 0"     >>= shouldBe "1"
      exprRepr "1 + -5"    >>= shouldBe "-4"

    it "Should multiply integers" $ do
      exprRepr "3 * 2"   >>= shouldBe "6"
      exprRepr "-1 * -1" >>= shouldBe "1"
      exprRepr "-3 * 2"  >>= shouldBe "-6"

    it "Should follow BEDMAS" $ do
      exprRepr "3 + 2 * 7"       >>= shouldBe "17"
      exprRepr "3 * 2 + 7"       >>= shouldBe "13"
      exprRepr "1 + 3 * 2 + 7"   >>= shouldBe "14"
      exprRepr "(1 + 3) * 2 + 7" >>= shouldBe "15"

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
      --exprRepr "wrap(3).applyl(Just(4))" >>= shouldBe "Just(3)"
  
    it "Maybe as a Monad" $ do
      exprRepr "Just(2).bind((x): Just(x + 10))" >>= shouldBe "Just(12)"
      exprRepr "Just(2).bind((x): None)"         >>= shouldBe "None"
      exprRepr "None.bind((x): Just(x))"         >>= shouldBe "None"
      exprRepr "None.bind((x): None)"            >>= shouldBe "None"
      exprRepr "join(Just(Just(1)))"             >>= shouldBe "Just(1)"
      exprRepr "join(None)"                      >>= shouldBe "None"

    it "Maybe as an Alternative" $ do
      exprRepr "Just(4).alt(Just(3))" >>= shouldBe "Just(4)"
      exprRepr "Just(4).alt(None)"    >>= shouldBe "Just(4)"
      exprRepr "None.alt(Just(3))"    >>= shouldBe "Just(3)"

    it "Maybe as a Traversable" $ do
      exprRepr "Just(3).traverse((x): [x, x + 1])" >>=
        shouldBe "[Just(3),Just(4)]"

    it "Maybe misc functions" $ do
      exprRepr "catMaybes([Just(3), Just(4), None, Just(7)])" >>=
        shouldBe "[3,4,7]"

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

  describe "List tests" $ do
    it "List as a Functor" $ do
      exprRepr "map([1, 2], add(1))" >>= shouldBe "[2,3]"
      exprRepr "map(Nil, add(1))" >>= shouldBe "[]"
      exprRepr "map(Cons(1, Cons(2, Nil)), add(1))" >>= shouldBe "[2,3]"

    it "List as a Foldable" $ do
      exprRepr "[1, 2, 3].foldl(0, (x, y): x + y)" >>= shouldBe "6"
      exprRepr "[].foldl(0, (x, y): x + y)"        >>= shouldBe "0"
      exprRepr "len([1, 2])"                       >>= shouldBe "2"

    it "List as a Monoid" $ do
      exprRepr "[1] + [3]"        >>= shouldBe "[1,3]"
      exprRepr "[1] <> [3]"       >>= shouldBe "[1,3]"
      exprRepr "\"ye\" <> \"et\"" >>= shouldBe "\"yeet\""

    it "List as a Traversable" $ do
      exprRepr "traverse([1, 2, 3], Just)"          >>= shouldBe "Just([1,2,3])"
      exprRepr "sequence([Just(1), Just(2), None])" >>= shouldBe "None"
      exprRepr "sequence([Just(1), Just(2)])"       >>= shouldBe "Just([1,2])"

    it "List misc functions" $ do
      exprRepr "[1, 2, 3].filter((x): x < 3)" >>= shouldBe "[1,2]"
      exprRepr "[1, 2, 3].head()"             >>= shouldBe "Just(1)"
      exprRepr "[3].tail()"                   >>= shouldBe "Just([])"
      exprRepr "[1, 2, 3].reverse()"          >>= shouldBe "[3,2,1]"
      exprRepr "[3].reverse()"                >>= shouldBe "[3]"

  describe "Inferrence tests" $ do
    it "Should infer wrap values" $ do
      exprRepr "[1].append(wrap(3))"                   >>= shouldBe "[1,3]"
      exprRepr "None.alt(wrap(3))"                     >>= shouldBe "Just(3)"
      exprRepr "[1, 2, 3].bind((x): wrap([x, x + 1]))" >>=
        shouldBe "[[1,2],[2,3],[3,4]]"

  describe "General tests" $ do
    it "Until" $ do
      exprRepr "until(-9, ((x): x > 0), ((x): x + 3))" >>= shouldBe "3"

    it "Misc char stuff" $ do
      exprRepr "ord('A')"      >>= shouldBe "65"
      exprRepr "chr(65)"       >>= shouldBe "'A'"
      exprRepr "chr(ord('A'))" >>= shouldBe "'A'"
      exprRepr "isDigit('a')"  >>= shouldBe "False"
      exprRepr "isDigit('8')"  >>= shouldBe "True"
      exprRepr "isDigit('/')"  >>= shouldBe "False"
      exprRepr "isDigit(':')"  >>= shouldBe "False"

    it "Misc bool stuff" $ do
      exprRepr "True and False" >>= shouldBe "False"
      exprRepr "True and True"  >>= shouldBe "True"
      exprRepr "True or False"  >>= shouldBe "True"

    it "Misc int stuff" $ do
      exprRepr "int(\"42\")" >>= shouldBe "42"
