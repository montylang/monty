import Test.Hspec
import Text.Parsec
import Text.Parsec.String
import Data.Either

import MontyParser

testParser :: (Indent -> Parser a) -> String -> Either ParseError a
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
      (testParser idParser "abc") `shouldBe` (Right $ "abc")
      (testParser idParser "_abc") `shouldBe` (Right $ "_abc")
      (testParser idParser "_abc123") `shouldBe` (Right $ "_abc123")
      (testParser idParser "1abc") `shouldSatisfy` isLeft

  describe "Int parser" $ do
    it "Parses ints" $ do
      (testParser exprParser "123") `shouldBe` (Right $ ExprInt 123)
      (testParser exprParser "123a") `shouldSatisfy` isLeft

  describe "Expr parser" $ do
    it "Infix ops" $ do
      (testParser exprParser "a + b") `shouldBe`
        (Right $ ExprInfix (ExprId "a") InfixAdd (ExprId "b"))

      (testParser exprParser "a + b + c") `shouldBe`
        (Right $ ExprInfix (ExprId "a") InfixAdd (ExprInfix (ExprId "b") InfixAdd (ExprId "c")))

    it "Paren eater" $ do
      (testParser exprParser "(a)") `shouldBe`
        (Right $ (ExprId "a"))

      (testParser exprParser "c + (a + b)") `shouldBe`
        (Right $ ExprInfix (ExprId "c") InfixAdd (ExprInfix (ExprId "a") InfixAdd (ExprId "b")))

      (testParser exprParser "(c + a) + b") `shouldBe`
        (Right $ ExprInfix (ExprInfix (ExprId "c") InfixAdd (ExprId "a")) InfixAdd (ExprId "b"))

  describe "Assignment parser" $ do
    it "Simple assignment" $ do
      (testParser assignmentParser "a = 3") `shouldBe`
        (Right $ ExprAssignment "a" (ExprInt 3))

  describe "Arg Parser" $ do
    it "Arg parsing" $ do
      (testParser defArgParser "( \t a,  b ,c ) :") `shouldBe`
        (Right $ [IdArg "a", IdArg "b", IdArg "c"])

  describe "Body parser" $ do
    it "Basic" $ do
      (testParser bodyParser $ " a = 3\n") `shouldBe`
        (Right $ [ExprAssignment "a" (ExprInt 3)])

      (testParser bodyParser $ unlines [" a = 3"]) `shouldBe`
        (Right $ [ExprAssignment "a" (ExprInt 3)])

    it "Multi" $ do
      (testParser bodyParser $ unlines ["  a = 3", "  a = 3"]) `shouldBe`
        (Right $ [ExprAssignment "a" (ExprInt 3), ExprAssignment "a" (ExprInt 3)])

      (testParser bodyParser $ unlines ["  a = 3", "", "", "  a = 3"]) `shouldBe`
        (Right $ [ExprAssignment "a" (ExprInt 3), ExprAssignment "a" (ExprInt 3)])

      (testParser bodyParser $ unlines ["  a = 3", "  a = 3", "  a = 3"]) `shouldBe`
        (Right $ [ExprAssignment "a" (ExprInt 3),
                  ExprAssignment "a" (ExprInt 3),
                  ExprAssignment "a" (ExprInt 3)])

    it "Inner def" $ do
      (testParser bodyParser $ unlines [
          "  def (x):",
          "    7",
          "  a = 5"
        ]) `shouldBe`
        (Right [
            (ExprDef [(IdArg "x")] [ExprInt 7]),
            (ExprAssignment "a" (ExprInt 5))
          ])

  -- OK Marvin, I like that
  describe "Root body parser" $ do
    it "Basic" $ do
      (parse rootBodyParser "" $ unlines ["a = 3"]) `shouldBe`
        (Right $ [ExprAssignment "a" (ExprInt 3)])

      (parse rootBodyParser "" $ unlines ["a = 3", "b = 4"]) `shouldBe`
        (Right $ [ExprAssignment "a" (ExprInt 3),
                  ExprAssignment "b" (ExprInt 4)])

    it "Exprs & defs" $ do
      (parse rootBodyParser "" $ unlines [
            "1",
            "def f(x):",
            "  x",
            "2"
          ]) `shouldBe`
        (Right $ [
           ExprInt 1,
           ExprAssignment "f" (ExprDef [IdArg "x"] [ExprId "x"]),
           ExprInt 2
         ])
      (parse rootBodyParser "" $ unlines ["if 3:", "  1", "else:", "  2"]) `shouldBe`
        (Right $ [
                   ExprIfElse
                   (CondBlock (ExprInt 3) [ExprInt 1])
                   []
                   [ExprInt 2]
                  ])

  describe "Cond block parser" $ do
    it "fake cond" $ do
      (testParser (condBlockParser "fake") $ unlines [
            "fake 3 :  # ain't real! ",
            "  4",
            "  5"
          ]) `shouldBe`
        (Right $ CondBlock (ExprInt 3) [ExprInt 4, ExprInt 5])

  describe "If parser" $ do
    it "If else" $ do
      (testParser exprParser $ unlines [
            "if 3:",
            "  4",
            "else:",
            "  5"
          ]) `shouldBe`
        (Right $ ExprIfElse
         (CondBlock (ExprInt 3) [ExprInt 4])
         []
         [ExprInt 5])

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
        (Right $ ExprIfElse
         (CondBlock (ExprInt 3) [ExprInt 4, ExprInt 5])
         [CondBlock (ExprInt 5) [ExprInt 6]]
         [ExprInt 7, ExprInt 8])

    it "If elif elif else" $ do
      (testParser exprParser $ unlines
       ["if 3:", "  4", "elif 5:", "  6", "elif 7:", "  8","else:", "  9"]) `shouldBe`
        (Right $ ExprIfElse
         (CondBlock (ExprInt 3) [ExprInt 4])
         [CondBlock (ExprInt 5) [ExprInt 6],
          CondBlock (ExprInt 7) [ExprInt 8]]
         [ExprInt 9])

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
        (Right $ (ExprIfElse
           (CondBlock
             (ExprInt 1)
             [ExprIfElse
               (CondBlock (ExprInt 2) [ExprInt 3])
               []
               [ExprInt 4]
             ]
           )
           []
           [ExprInt 5])
         )
 
  describe "Def" $ do
    it "Anonymous def" $ do
      (testParser exprParser $ unlines ["def (a, b)  : ", "  4"]) `shouldBe`
        (Right $ ExprDef
         [(IdArg "a"), (IdArg "b")]
         [ExprInt 4])

      (testParser exprParser $ unlines ["def():", "  4"]) `shouldBe`
        (Right $ ExprDef [] [ExprInt 4])

    it "Named def" $ do
      (testParser exprParser $ unlines ["def foo():", "  4"]) `shouldBe`
        (Right $ ExprAssignment "foo" $ ExprDef [] [ExprInt 4])

  describe "Calling functions" $ do
    it "Named function call" $ do
      (testParser exprParser $ "foo(x)") `shouldBe`
        (Right $ ExprCall (ExprId "foo") [(ExprId "x")])

      (testParser exprParser $ "foo()") `shouldBe`
        (Right $ ExprCall (ExprId "foo") [])

      (testParser exprParser $ "(1 + 2)()") `shouldBe`
        (Right $ (ExprCall (ExprInfix (ExprInt 1) InfixAdd (ExprInt 2)) []))

      (testParser exprParser $ "(foo())()") `shouldBe`
        (Right $ ExprCall (ExprCall (ExprId "foo") []) [])

    it "Curried function call" $ do
      (testParser exprCallParser $ "foo()()") `shouldBe`
        (Right $ ExprCall (ExprCall (ExprId "foo") []) [])
