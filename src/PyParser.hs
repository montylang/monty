module PyParser where

import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.List
import Debug.Trace

type Id = String
type Indent = String

data CondBlock = CondBlock Expr [Expr]
  deriving (Show, Eq)

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
eol = ws *> optional commentEater *> char '\n' *> pure ()

-- TODO: Maybe just define this as an EOL, consume them all!!
eol1 :: Parser()
eol1 = eol <* (many $ try eol)

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
defArgParser indent = multiParenParser (argParser indent) <* ws <* char ':'

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
    char '(' *> delimWs *>
    sepBy (delimWs *> innerParser <* delimWs) (char ',')
    <* delimWs <* char ')'
  where
    delimWs = many $ oneOf "\t \n"

defParser :: Indent -> Parser Expr
defParser indent = do
  _    <- string "def"
  _    <- ws
  args <- defArgParser indent
  _    <- eol
  body <- trace "made to body parser" $ bodyParser indent
  pure $ trace "finished body parser" $ ExprDef args body

-- TODO: Support this `if foo: print('reeee')`
condBlockParser :: String -> Indent -> Parser CondBlock
condBlockParser initialKeyword indent = do
  _    <- try (string initialKeyword <* ws1)
  cond <- exprParser indent
  _    <- ws
  _    <- char ':'
  _    <- eol1
  body <- bodyParser indent
  _    <- eol1
  pure $ CondBlock cond body

ifParser :: Indent -> Parser Expr
ifParser indent = do
    fi   <- condBlockParser "if" indent 
    file <- elifsParser
    esle <- elseParser
    pure $ ExprIfElse fi file esle
  where
    elifsParser :: Parser [CondBlock]
    elifsParser = many (try $ condBlockParser "elif" indent)

    elseParser :: Parser [Expr]
    elseParser = elseBoiler *> bodyParser indent

    elseBoiler :: Parser ()
    elseBoiler = string indent *> string "else" *> ws *> char ':' *> eol *> pure ()

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
exprParser indent = choice [
    ifParser indent,
    try $ defParser indent,
    try $ namedDefParser indent,
    try $ infixParser indent,
    try $ assignmentParser indent,
    try $ exprCallParser indent,
    exprParser' indent
  ]

-- Pretty epic
bodyParser :: Indent -> Parser [Expr]
bodyParser base = do
    nextIndent <- (<>) <$> string base <*> ws1
    first      <- exprParser nextIndent <* lookAhead eol
    rest       <- many $ try $ stmt nextIndent
    pure $ first:rest
  where
    stmt :: Indent -> Parser Expr
    stmt nextIndent = eol1 *> string nextIndent *> exprParser nextIndent

-- FIXME: Make this less waste
rootBodyParser :: Parser [Expr]
rootBodyParser = do
    first      <- exprParser "" <* lookAhead eol
    rest       <- many $ try $ stmt
    pure $ first:rest
  where
    stmt :: Parser Expr
    stmt = eol1 *> string "" *> exprParser ""
