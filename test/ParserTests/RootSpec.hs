module ParserTests.RootSpec where

import Test.Hspec
import Text.Megaparsec
import Data.Either

import ParserTypes
import Parser.Root

import ParserTests.Helpers

spec :: Spec
spec = do
  describe "Import parser" $ do
    it "Parses imports" $ do
      (parse (importParser <* eof) "" "import foo") `shouldBe`
        (Right $ pure $ ExprImport ["foo"])
      (parse (importParser <* eof) "" "import foo.bar.it") `shouldBe`
        (Right $ pure $ ExprImport ["foo", "bar", "it"])
      (parse (importParser <* eof) "" "import foo.") `shouldSatisfy` isLeft

  describe "Int parser" $ do
    it "Parses ints" $ do
      (testParser exprParser "123") `shouldBe` (Right $ pure $ ExprInt 123)
      (testParser exprParser "123a") `shouldSatisfy` isLeft

  describe "String parser" $ do
    it "Parses strings" $ do
      (testParser exprParser "\"test\"") `shouldBe` (Right $ pure $ mkStrExpr "test")
      (testParser exprParser "\"'''\"") `shouldBe` (Right $ pure $ mkStrExpr "'''")
      (testParser exprParser "'test'") `shouldSatisfy` isLeft
      (testParser exprParser "'\"\"\"'") `shouldSatisfy` isLeft
      (testParser exprParser "\"test'") `shouldSatisfy` isLeft
      (testParser exprParser "'test\"") `shouldSatisfy` isLeft

  describe "Char parser" $ do
    it "Parses chars" $ do
      (testParser exprParser "'c'") `shouldBe` (Right $ pure $ ExprChar 'c')
      (testParser exprParser "''") `shouldSatisfy` isLeft
      (testParser exprParser "'cc'") `shouldSatisfy` isLeft

  describe "List parser" $ do
    it "Parses lists" $ do
      (testParser exprParser "[1, 2]") `shouldBe`
        (Right $ pure $ ExprList [pure $ ExprInt 1, pure $ ExprInt 2])
      (testParser exprParser "[\"a\", \"b\"]") `shouldBe`
        (Right $ pure $ ExprList [pure $ mkStrExpr "a", pure $ mkStrExpr "b"])
      (testParser exprParser "[1, \"b\"]") `shouldBe`
        (Right $ pure $ ExprList [pure $ ExprInt 1, pure $ mkStrExpr "b"])
      (testParser exprParser "[1, ]") `shouldSatisfy` isLeft

  describe "Tuple parser" $ do
    it "Parses Tuples" $ do
      (testParser exprParser "()") `shouldBe`
        (Right $ pure $ ExprTuple [])

      (testParser exprParser "(1, 2)") `shouldBe`
        (Right $ pure $ ExprTuple [pure $ ExprInt 1, pure $ ExprInt 2])

      (testParser exprParser "(1, 2, 3 )") `shouldBe`
        (Right $ pure $ ExprTuple [
            pure $ ExprInt 1,
            pure $ ExprInt 2,
            pure $ ExprInt 3])

      (testParser exprParser "(1)") `shouldBe`
        (Right $ pure $ ExprInt 1)

  describe "Infix parser" $ do
    it "Simple infix ops" $ do
      (testParser exprParser "a + b") `shouldBe`
        (Right $ pure $ ExprInfix (pure $ ExprId "a") InfixAdd (pure $ ExprId "b"))

      (testParser exprParser "a + b + c") `shouldBe`
        (Right $ pure $ ExprInfix
         (pure $ ExprInfix (pure $ ExprId "a") InfixAdd (pure $ ExprId "b"))
         InfixAdd
         (pure $ ExprId "c"))

    it "Precedence" $ do
      (testParser exprParser "a + b * c") `shouldBe`
        (Right $ pure $ ExprInfix
         (pure $ ExprId "a")
         InfixAdd
         (pure $ ExprInfix (pure $ ExprId "b") InfixMul (pure $ ExprId "c")))

      (testParser exprParser "a + b * c + d") `shouldBe`
        (Right $ pure $
            ExprInfix
                (pure $ ExprInfix
                            (pure $ ExprId "a")
                            InfixAdd
                            (pure $ ExprInfix
                                    (pure $ ExprId "b")
                                    InfixMul
                                    (pure $ ExprId "c")))
                InfixAdd
                (pure $ ExprId "d"))

  describe "Expr parser" $ do
    it "Paren eater" $ do
      (testParser exprParser "(a)") `shouldBe`
        (Right $ pure $ (ExprId "a"))

      (testParser exprParser "c + (a + b)") `shouldBe`
        (Right $ pure $ ExprInfix
         (pure $ ExprId "c")
         InfixAdd
         (pure $ ExprInfix (pure $ ExprId "a") InfixAdd (pure $ ExprId "b")))

      (testParser exprParser "(c + a) + b") `shouldBe`
        (Right $ pure $ ExprInfix
         (pure $ ExprInfix (pure $ ExprId "c") InfixAdd (pure $ ExprId "a"))
         InfixAdd
         (pure $ ExprId "b"))

  describe "Assignment parser" $ do
    it "Simple assignment" $ do
      (testParser assignmentParser "a = 3") `shouldBe`
        (Right $ pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3))

  describe "Body parser" $ do
    it "Basic" $ do
      (testParser bodyParser $ " a = 3\n") `shouldBe`
        (Right $ [pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3)])

      (testParser bodyParser $ unlines [" a = 3"]) `shouldBe`
        (Right $ [pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3)])

    it "Multi" $ do
      (testParser bodyParser $ unlines ["  a = 3", "  a = 3"]) `shouldBe`
        (Right $ [
            pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3),
            pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3)])

      (testParser bodyParser $ unlines ["  a = 3", "", "", "  a = 3"]) `shouldBe`
        (Right $ [
            pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3),
            pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3)])

      (testParser bodyParser $ unlines ["  a = 3", "  a = 3", "  a = 3"]) `shouldBe`
        (Right $ [pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3),
                  pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3),
                  pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3)])

    it "Inner def" $ do
      (testParser bodyParser $ unlines [
          "  def (x):",
          "    7",
          "  a = 5"
        ]) `shouldBe`
        (Right [
            pure $ (ExprDef [(IdArg "x")] [pure $ ExprInt 7]),
            pure $ (ExprAssignment (IdArg "a") (pure $ ExprInt 5))
          ])

  -- OK Marvin, I like that
  describe "Root body parser" $ do
    it "Basic" $ do
      (parse rootBodyParser "" $ unlines ["a = 3"]) `shouldBe`
        (Right $ [pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3)])

      (parse rootBodyParser "" $ unlines ["a = 3", "b = 4"]) `shouldBe`
        (Right $ [pure $ ExprAssignment (IdArg "a") (pure $ ExprInt 3),
                  pure $ ExprAssignment (IdArg "b") (pure $ ExprInt 4)])

    it "Exprs & defs" $ do
      (parse rootBodyParser "" $ unlines [
            "1",
            "def f(x):",
            "  x",
            "2"
          ]) `shouldBe`
        (Right $ [
           pure $ ExprInt 1,
           pure $ ExprAssignment
                    (IdArg "f")
                    (pure $ ExprDef [IdArg "x"] [pure $ ExprId "x"]),
           pure $ ExprInt 2
         ])
      (parse rootBodyParser "" $ unlines ["if 3:", "  1", "else:", "  2"]) `shouldBe`
        (Right $ [
                   pure $ ExprIfElse
                   (CondBlock (pure $ ExprInt 3) [pure $ ExprInt 1])
                   []
                   [pure $ ExprInt 2]
                  ])

  describe "Cond block parser" $ do
    it "fake cond" $ do
      (testParser (condBlockParser "fake") $ unlines [
            "fake 3 :  # ain't real! ",
            "  4",
            "  5"
          ]) `shouldBe`
        (Right $ CondBlock (pure $ ExprInt 3) [pure $ ExprInt 4, pure $ ExprInt 5])

  describe "If parser" $ do
    it "If else" $ do
      (testParser exprParser $ unlines [
            "if 3:",
            "  4",
            "else:",
            "  5"
          ]) `shouldBe`
        (Right $ pure $ ExprIfElse
         (CondBlock (pure $ ExprInt 3) [pure $ ExprInt 4])
         []
         [pure $ ExprInt 5])

    it "If elif else" $ do
      (testParser exprParser $ unlines [
            "if 3:",
            "  4",
            "  5",
            "elif 5:",
            "  6",
            "else:",
            "  7",
            "  8"
          ]) `shouldBe`
        (Right $ pure $ ExprIfElse
         (CondBlock (pure $ ExprInt 3) [pure $ ExprInt 4, pure $ ExprInt 5])
         [CondBlock (pure $ ExprInt 5) [pure $ ExprInt 6]]
         [pure $ ExprInt 7, pure $ ExprInt 8])

    it "If elif elif else" $ do
      (testParser exprParser $ unlines
       ["if 3:", "  4", "elif 5:", "  6", "elif 7:", "  8","else:", "  9"]) `shouldBe`
        (Right $ pure $ ExprIfElse
         (CondBlock (pure $ ExprInt 3) [pure $ ExprInt 4])
         [CondBlock (pure $ ExprInt 5) [pure $ ExprInt 6],
          CondBlock (pure $ ExprInt 7) [pure $ ExprInt 8]]
         [pure $ ExprInt 9])

    it "Nested if/else" $ do
      (testParser exprParser $ unlines [
          "if 1:",
          "  if 2:",
          "    3",
          "  else:",
          "    4",
          "else:",
          "  5"]
        ) `shouldBe`
        (Right $ pure $ (ExprIfElse
           (CondBlock
             (pure $ ExprInt 1)
             [pure $ ExprIfElse
               (CondBlock (pure $ ExprInt 2) [pure $ ExprInt 3])
               []
               [pure $ ExprInt 4]
             ]
           )
           []
           [pure $ ExprInt 5])
         )

  describe "Def" $ do
    it "Anonymous def" $ do
      (testParser exprParser $ unlines ["def (a, b)  : ", "  4"]) `shouldBe`
        (Right $ pure $ ExprDef
         [(IdArg "a"), (IdArg "b")]
         [pure $ ExprInt 4])

      (testParser exprParser $ unlines ["def():", "  4"]) `shouldBe`
        (Right $ pure $ ExprDef [] [pure $ ExprInt 4])

    it "Named def" $ do
      (testParser exprParser $ unlines ["def foo():", "  4"]) `shouldBe`
        (Right $ pure $ ExprAssignment
         (IdArg "foo") $
         pure $ ExprDef [] [pure $ ExprInt 4]
        )

    it "Lambda" $ do
      (testParser exprParser $ unlines ["(x): x + 4"]) `shouldBe`
        (Right $ pure $
         ExprDef
           [IdArg "x"]
           [pure $ ExprReturn
             (pure $ ExprInfix (pure $ ExprId "x") InfixAdd (pure $ ExprInt 4))
           ])

  describe "Return" $ do
    it "Returns" $ do
      (testParser exprParser $ unlines ["return 1"]) `shouldBe`
        (Right $ pure $ ExprReturn $ pure $ ExprInt 1)

      (testParser exprParser $ unlines ["def (x):", "  return x"]) `shouldBe`
        (Right $ pure $
         (ExprDef [(IdArg "x")] [pure $ ExprReturn $ pure $ ExprId "x"]))

  describe "Calling functions" $ do
    it "Named function call" $ do
      (testParser exprParser $ "foo(x)") `shouldBe`
        (Right $ pure $ ExprCall (pure $ ExprId "foo") [(pure $ ExprId "x")])

      (testParser exprParser $ "foo()") `shouldBe`
        (Right $ pure $ ExprCall (pure $ ExprId "foo") [])

      (testParser exprParser $ "(1 + 2)()") `shouldBe`
        (Right $ pure $
         (ExprCall
          (pure $ ExprInfix (pure $ ExprInt 1) InfixAdd (pure $ ExprInt 2))
          []))

      (testParser exprParser $ "(foo())()") `shouldBe`
        (Right $ pure $ ExprCall (pure $ ExprCall (pure $ ExprId "foo") []) [])

    it "Curried function call" $ do
      (testParser exprParser $ "foo()()") `shouldBe`
        (Right $ pure $ ExprCall (pure $ ExprCall (pure $ ExprId "foo") []) [])

    it "Sugar syntax" $ do
      (testParser exprParser $ "foo.bar(it)") `shouldBe`
        (Right $ pure $
            ExprCall (pure $ ExprId "bar") [pure $ ExprId "foo", pure $ ExprId "it"])

  describe "Classes but really just data" $ do
    it "Constructs" $ do
      (testParser exprParser $ unlines [
            "class Maybe:",
            "  None()",
            "  Just(you)"
          ]) `shouldBe`
        (Right $ pure $ ExprClass "Maybe" [
              pure $ TypeCons "None" [],
              pure $ TypeCons "Just" ["you"]
            ])

  describe "Instances but really just instances" $ do
    it "Instances" $ do
      (testParser instanceParser $ unlines [
            "instance Maybe of Functor:",
            "  def bar():",
            "    return 0"
          ]) `shouldBe`
        (Right $ pure $ ExprInstanceOf "Maybe" "Functor" [
              pure $ ExprAssignment
                (IdArg "bar")
                (pure $ ExprDef [] [pure $ ExprReturn (pure $ ExprInt 0)])
            ])

  describe "Unwrap parser" $ do
    it "Parses uwrap statements" $ do
      (testParser unwrapParser $ unlines [
            "unwrap:",
            "  foo <- bar",
            "  it"
          ]) `shouldBe`
        (Right $ pure $ ExprUnwrap [
              pure $ ExprBind (IdArg "foo") (pure $ ExprId "bar"),
              pure $ ExprId "it"
            ])

      (testParser unwrapParser $ unlines [
            "unwrap:",
            "  foo <- bar",
            "  it",
            "  larry",
            "  curly <- 3 + 3"
          ]) `shouldBe`
        (Right $ pure $ ExprUnwrap [
              pure $ ExprBind (IdArg "foo") (pure $ ExprId "bar"),
              pure $ ExprId "it",
              pure $ ExprId "larry",
              pure $ ExprBind (IdArg "curly")
                (pure $ ExprInfix (pure $ ExprInt 3) InfixAdd (pure $ ExprInt 3))
            ])

      (testParser unwrapParser $ unlines [
            "unwrap:",
            "  (a, b) <- bar"
          ]) `shouldBe`
        (Right $ pure $ ExprUnwrap [
              pure $ ExprBind
                (PatternArg "Tuple" [IdArg "a", IdArg "b"])
                (pure $ ExprId "bar")
            ])
