import Test.Hspec
import Text.Megaparsec
import Data.Either
import Data.Void

import ParserTypes
import MontyParser

testParser :: (Indent -> Parser a) -> String -> Either (ParseErrorBundle String Void) a
testParser p input = parse (p "" <* (try $ many $ try singleEol) <* eof) "" input

main :: IO ()
main = hspec $ do
  describe "SingleEol parser" $ do
    it "Parses EOLs" $ do
      parse (singleEol <* eof) "" "  \n" `shouldBe` (Right ()) 
      parse (singleEol <* eof) "" "  # \n" `shouldBe` (Right ()) 
      parse (singleEol <* eof) "" "  #  sntaoehusnthoea \n" `shouldBe` (Right ()) 

  describe "Id parser" $ do
    it "Parses ids" $ do
      (testParser varIdParser "abc") `shouldBe` (Right $ "abc")
      (testParser varIdParser "_abc") `shouldBe` (Right $ "_abc")
      (testParser varIdParser "_abc123") `shouldBe` (Right $ "_abc123")
      (testParser varIdParser "1abc") `shouldSatisfy` isLeft

  describe "Int parser" $ do
    it "Parses ints" $ do
      (testParser exprParser "123") `shouldBe` (Right $ pure $ ExprInt 123)
      (testParser exprParser "123a") `shouldSatisfy` isLeft

  describe "String parser" $ do
    it "Parses strings" $ do
      (testParser exprParser "\"test\"") `shouldBe` (Right $ pure $ ExprString "test")
      (testParser exprParser "'test'") `shouldBe` (Right $ pure $ ExprString "test")
      (testParser exprParser "'\"\"\"'") `shouldBe` (Right $ pure $ ExprString "\"\"\"")
      (testParser exprParser "\"'''\"") `shouldBe` (Right $ pure $ ExprString "'''")
      (testParser exprParser "\"test'") `shouldSatisfy` isLeft
      (testParser exprParser "'test\"") `shouldSatisfy` isLeft

  describe "Expr parser" $ do
    it "Infix ops" $ do
      (testParser exprParser "a + b") `shouldBe`
        (Right $ pure $ ExprInfix (pure $ ExprId "a") InfixAdd (pure $ ExprId "b"))

      (testParser exprParser "a + b + c") `shouldBe`
        (Right $ pure $ ExprInfix
         (pure $ ExprId "a")
         InfixAdd
         (pure $ ExprInfix (pure $ ExprId "b") InfixAdd (pure $ ExprId "c")))

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
        (Right $ pure $ ExprAssignment "a" (pure $ ExprInt 3))

  describe "Arg Parser" $ do
    it "Arg parsing" $ do
      (testParser defArgParser "( \t a,  b ,c )") `shouldBe`
        (Right $ [IdArg "a", IdArg "b", IdArg "c"])

    it "Pattern matching" $ do
      (testParser defArgParser "(Just(a), None())") `shouldBe`
        (Right $ [PatternArg "Just" [IdArg "a"], PatternArg "None" []])

  describe "Body parser" $ do
    it "Basic" $ do
      (testParser bodyParser $ " a = 3\n") `shouldBe`
        (Right $ [pure $ ExprAssignment "a" (pure $ ExprInt 3)])

      (testParser bodyParser $ unlines [" a = 3"]) `shouldBe`
        (Right $ [pure $ ExprAssignment "a" (pure $ ExprInt 3)])

    it "Multi" $ do
      (testParser bodyParser $ unlines ["  a = 3", "  a = 3"]) `shouldBe`
        (Right $ [
            pure $ ExprAssignment "a" (pure $ ExprInt 3),
            pure $ ExprAssignment "a" (pure $ ExprInt 3)])

      (testParser bodyParser $ unlines ["  a = 3", "", "", "  a = 3"]) `shouldBe`
        (Right $ [
            pure $ ExprAssignment "a" (pure $ ExprInt 3),
            pure $ ExprAssignment "a" (pure $ ExprInt 3)])

      (testParser bodyParser $ unlines ["  a = 3", "  a = 3", "  a = 3"]) `shouldBe`
        (Right $ [pure $ ExprAssignment "a" (pure $ ExprInt 3),
                  pure $ ExprAssignment "a" (pure $ ExprInt 3),
                  pure $ ExprAssignment "a" (pure $ ExprInt 3)])

    it "Inner def" $ do
      (testParser bodyParser $ unlines [
          "  def (x):",
          "    7",
          "  a = 5"
        ]) `shouldBe`
        (Right [
            pure $ (ExprDef [(IdArg "x")] [pure $ ExprInt 7]),
            pure $ (ExprAssignment "a" (pure $ ExprInt 5))
          ])

  -- OK Marvin, I like that
  describe "Root body parser" $ do
    it "Basic" $ do
      (parse rootBodyParser "" $ unlines ["a = 3"]) `shouldBe`
        (Right $ [pure $ ExprAssignment "a" (pure $ ExprInt 3)])

      (parse rootBodyParser "" $ unlines ["a = 3", "b = 4"]) `shouldBe`
        (Right $ [pure $ ExprAssignment "a" (pure $ ExprInt 3),
                  pure $ ExprAssignment "b" (pure $ ExprInt 4)])

    it "Exprs & defs" $ do
      (parse rootBodyParser "" $ unlines [
            "1",
            "def f(x):",
            "  x",
            "2"
          ]) `shouldBe`
        (Right $ [
           pure $ ExprInt 1,
           pure $ ExprAssignment "f" (pure $ ExprDef [IdArg "x"] [pure $ ExprId "x"]),
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
        (Right $ pure $ ExprAssignment "foo" $ pure $ ExprDef [] [pure $ ExprInt 4])

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
      (testParser exprCallParser $ "foo()()") `shouldBe`
        (Right $ pure $ ExprCall (pure $ ExprCall (pure $ ExprId "foo") []) [])

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
                "bar"
                (pure $ ExprDef [] [pure $ ExprReturn (pure $ ExprInt 0)])
            ])
