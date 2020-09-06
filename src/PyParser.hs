module PyParser where

import Text.Parsec
import Text.Parsec.String
import Data.Char

-- import foo
-- from foo import bar
-- from foo.bar.it import Baz

type Id = String

data CondBlock = CondBlock Expr [Expr]
  deriving (Show, Eq)

{-
data Stmt
  = StmtImportBasic String
  | StmtImportFrom String String
  | StmtAssignment Id Expr
  | StmtExpr Expr
  deriving (Show, Eq)
-}

data InfixOp
  = InfixAdd
  | InfixSub
  | InfixMul
  | InfixDiv
  | InfixMod
  | InfixEq
  | InfixNotEq
  | InfixGreater
  | InfixLess
  | InfixLessEqual
  | InfixGreaterEqual
  | InfixLogicAnd
  | InfixLogicOr
  deriving (Show, Eq)

data Arg = IdArg Id
  deriving (Show, Eq)

data Expr
  = ExprId Id
  | ExprInt Int
  | ExprIfElse CondBlock [CondBlock] [Expr]
  | ExprInfix Expr InfixOp Expr
  | ExprAssignment Id Expr
  | ExprDef [Arg] [Expr]
  | ExprCall Expr [Expr]
  deriving (Show, Eq)

ws :: Parser String
ws = many $ char ' '

ws1 :: Parser String
ws1 = many1 $ char ' '

commentEater :: Parser ()
commentEater = char '#' *> many (noneOf "\n") *> pure ()

-- TODO: Support CRLF and ;
eol :: Parser ()
eol = try reol-- <|> eof
  where
    reol = ws *> optional commentEater *> char '\n' *> pure ()

moduleParser :: Parser String
moduleParser = many1 $ alphaNum <|> char '.'

assignmentParser :: Indent -> Parser Expr
assignmentParser indent = do
  var  <- idParser indent
  _    <- ws
  _    <- char '='
  _    <- ws
  expr <- exprParser indent
  pure $ ExprAssignment var expr

-- TODO: Support pattern matching
argParser :: Indent -> Parser Arg
argParser indent = IdArg <$> idParser indent

defArgParser :: Indent -> Parser [Arg]
defArgParser indent = multiParenParser (argParser indent) <* char ':'

namedDefParser :: Indent -> Parser Expr
namedDefParser indent = do
  _    <- string "def"
  _    <- ws1
  name <- idParser indent
  args <- defArgParser indent
  _    <- eol
  body <- bodyParser indent
  pure $ ExprAssignment name $ ExprDef args body

-- ExprCall Expr [Expr]

-- Supports f() and f()()
exprCallParser :: Indent -> Parser Expr
exprCallParser indent = do
  fun <- exprParser' indent
  firstArgLists <- multiParenParser $ exprParser indent
  otherArgLists <- many $ multiParenParser $ exprParser indent
  pure $ foldl ExprCall (ExprCall fun firstArgLists) otherArgLists

-- Matches syntax of the form (anything, anything, ...)
multiParenParser :: Parser a -> Parser [a]
multiParenParser innerParser =
    ws *> char '(' *>
    sepBy innerParser delimParser
    <* char ')'
  where
    delimParser = ws *> char ',' *> delimWs *> pure ()
    delimWs = many $ oneOf " \t\n"

defParser :: Indent -> Parser Expr
defParser indent = do
  _    <- string "def"
  args <- defArgParser indent
  _    <- eol
  body <- bodyParser indent
  pure $ ExprDef args body

-- TODO: Support this `if foo: print('reeee')`
condBlockParser :: Indent -> String -> Parser CondBlock
condBlockParser indent initialKeyword = do
  _ <- string initialKeyword
  _ <- ws1
  cond <- exprParser indent
  _ <- ws
  _ <- char ':'
  _ <- eol
  body <- bodyParser indent
  pure $ CondBlock cond body

ifParser :: Indent -> Parser Expr
ifParser indent = do
    fi   <- condBlockParser indent "if"
    file <- elifsParser
    esle <- elseParser
    pure $ ExprIfElse fi file esle
  where
    elifsParser :: Parser [CondBlock]
    elifsParser = many $ try $ condBlockParser indent "elif"

    elseParser :: Parser [Expr]
    elseParser = elseBoiler *> bodyParser indent

    elseBoiler :: Parser ()
    elseBoiler = string "else" *> ws *> char ':' *> (many1 $ try eol) *> pure ()

-- Pretty epic
infixOpParser :: Parser InfixOp
infixOpParser = choice $ op <$> arr
  where
    op :: (String, InfixOp) -> Parser InfixOp
    op (s, o) = string s *> pure o
    arr = [
        ("+", InfixAdd),
        ("-", InfixSub),
        ("*", InfixMul),
        ("/", InfixDiv),
        ("%", InfixMod),
        ("==", InfixEq),
        ("!=", InfixNotEq),
        (">", InfixGreater),
        ("<", InfixLess),
        ("<=", InfixLessEqual),
        (">=", InfixGreaterEqual),
        ("&&", InfixLogicAnd),
        ("and", InfixLogicAnd),
        ("||", InfixLogicOr),
        ("or", InfixLogicOr)
      ]

infixParser :: Indent -> Parser Expr
infixParser indent = do
  first  <- exprParser' indent
  _      <- ws
  op     <- infixOpParser
  _      <- ws
  second <- exprParser indent
  pure $ ExprInfix first op second

idParser :: Indent -> Parser Id
idParser _ = do
  x  <- char '_' <|> satisfy isAlpha
  xs <- many $ (char '_' <|> alphaNum)
  pure $ x:xs

exprIdParser :: Indent -> Parser Expr
exprIdParser indent = ExprId <$> idParser indent

intParser :: Indent -> Parser Expr
intParser _ = ExprInt <$> read <$> many1 digit

parenEater :: Parser Expr -> Parser Expr
parenEater innerParser =
  char '(' *> ws *> innerParser <* ws <* char ')'

exprParser' :: Indent -> Parser Expr
exprParser' indent = try (parenEater $ exprParser indent) <|> content
  where content = choice $ try <$> ($ indent) <$> [
            exprIdParser,
            intParser
          ]

exprParser :: Indent -> Parser Expr
exprParser indent = choice $ try <$> ($ indent) <$> [
    ifParser,
    defParser,
    namedDefParser,
    infixParser,
    assignmentParser,
    exprCallParser,
    exprParser'
  ]

type Indent = String

-- FIXME: Support multiple eols after a statement
-- Hot
{-
bodyParser :: Indent -> Parser [Expr]
bodyParser base = do
    nIndent <- firstIndent
    first   <- exprParser nIndent
    _       <- many1 $ try eol
    rest    <- extraStatements nIndent
    pure (first:rest)
  where
    extraStatements :: Indent -> Parser [Expr]
    extraStatements indent =
      many $ string indent *> exprParser indent <* eol

    firstIndent = do
      headIndent <- string base
      tailIndent <- ws1
      pure $ headIndent <> tailIndent
-}
bodyParser :: Indent -> Parser [Expr]
bodyParser base = do
    nIndent <- firstIndent
    first   <- exprParser nIndent
    _       <- many1 $ try eol
    rest    <- extraStatements nIndent
    pure (first:rest)
  where
    extraStatements :: Indent -> Parser [Expr]
    extraStatements indent =
      many $ string indent *> exprParser indent <* eol

    firstIndent = do
      headIndent <- string base
      tailIndent <- ws1
      pure $ headIndent <> tailIndent

rootBodyParser :: Parser [Expr]
rootBodyParser = sepBy (exprParser "" <* eol) eol <* eof
--rootBodyParser = sepBy (exprParser "" <* eol) (many $ try eol) <* eof
--sepBy innerParser delimParser
--rootBodyParser = (many $ exprParser "" <* (many1 $ try eol)) <* eof
