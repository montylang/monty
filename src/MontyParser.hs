module MontyParser where

import Text.Parsec
import Text.Parsec.String
import Data.Char
import Debug.Trace

type Id = String
type Indent = String

data CondBlock = CondBlock Expr [Expr]
  deriving (Show, Eq)

data InfixOp
  = InfixAdd  -- Semiring add(a, b): semiring
  | InfixSub  -- Ring
  | InfixMul  -- Semiring
  | InfixDiv
  | InfixMod  -- Semiring
  | InfixEq       -- Equal equal(a, b): bool
  | InfixNotEq    -- Equal
  | InfixGreater  -- Ord : ord(a, b) == GT
  | InfixLess
  | InfixLessEqual
  | InfixGreaterEqual
  | InfixLogicAnd -- Logic: lor(a, b): bool
  | InfixLogicOr
  | InfixCons
  deriving (Show, Eq)

data Arg
  = IdArg Id
  | PatternArg Id [Arg]
  deriving (Show, Eq)

data Expr
  = ExprId Id
  | ExprInt Int
  | ExprString String
  | ExprIfElse CondBlock [CondBlock] [Expr]
  | ExprInfix Expr InfixOp Expr
  | ExprAssignment Id Expr
  | ExprDef [Arg] [Expr]
  | ExprCall Expr [Expr]
  | ExprReturn Expr
  | ExprClass Id [TypeCons]
  | ExprList [Expr]
  | ExprType Id [TypeHeader]
  deriving (Show, Eq)

-- TODO: [TypeAnnotation]
data TypeHeader = TypeHeader Id [Id]
  deriving (Show, Eq)

data TypeCons = TypeCons Id [Arg]
  deriving (Show, Eq)

ws :: Parser String
ws = many $ char ' '

ws1 :: Parser String
ws1 = many1 $ char ' '

commentEater :: Parser ()
commentEater = char '#' *> many (noneOf "\n") *> pure ()

-- TODO: Support CRLF and ;
singleEol :: Parser ()
singleEol = ws *> optional commentEater *> char '\n' *> pure ()

eol1 :: Parser ()
eol1 = singleEol <* (many $ try singleEol)

moduleParser :: Parser String
moduleParser = many1 $ alphaNum <|> char '.'

assignmentParser :: Indent -> Parser Expr
assignmentParser indent = do
  var  <- idParser indent
  _    <- ws <* char '=' <* ws
  expr <- exprParser indent
  pure $ ExprAssignment var expr

-- TODO: Support pattern matching
argParser :: Indent -> Parser Arg
argParser indent = choice $ try <$> [
    consArgParser, -- Order matters
    patternArgParser,
    idArgParser
  ]
  where
    idArgParser :: Parser Arg
    idArgParser = IdArg <$> idParser indent

    patternArgParser :: Parser Arg
    patternArgParser = do
      name <- idParser indent <* ws
      args <- defArgParser indent
      pure $ PatternArg name args

    consArgParser :: Parser Arg
    consArgParser = do
      headArg <- try $ idArgParser <* ws <* char '|' <* ws
      tailArg <- try idArgParser <|> try consArgParser
      pure $ PatternArg "Cons" [headArg, tailArg]

defArgParser :: Indent -> Parser [Arg]
defArgParser indent = multiParenParser '(' ')' (argParser indent) <* ws

namedDefParser :: Indent -> Parser Expr
namedDefParser indent = do
  name <- string "def" *> ws1 *> idParser indent
  args <- defArgParser indent <* char ':' <* eol1
  body <- bodyParser indent
  pure $ ExprAssignment name $ ExprDef args body

consParser :: Indent -> Parser Expr
consParser indent = do
  headExpr <- try $ exprParser' indent <* ws <* char '|' <* ws
  tailExpr <- exprParser indent
  pure $ ExprCall (ExprId "Cons") [headExpr, tailExpr]

-- Supports f() and f()()
exprCallParser :: Indent -> Parser Expr
exprCallParser indent = do
  fun           <- exprParser' indent
  firstArgLists <- multiParenParser '(' ')' $ exprParser indent
  otherArgLists <- many $ multiParenParser '(' ')' $ exprParser indent
  pure $ foldl ExprCall (ExprCall fun firstArgLists) otherArgLists

-- Eats surrounding parens of an expr, for disambiguation
parenEater :: Parser Expr -> Parser Expr
parenEater innerParser =
    char '(' *> ws *> innerParser <* ws <* char ')'

-- Matches syntax of the form (anything, anything, ...)
multiParenParser :: Char -> Char -> Parser a -> Parser [a]
multiParenParser open close innerParser =
    char open *> delimWs *>
    sepBy (delimWs *> innerParser <* delimWs) (char ',')
    <* delimWs <* char close
  where
    delimWs = many $ oneOf "\t \n"

defParser :: Indent -> Parser Expr
defParser indent = do
  _    <- try $ string "def" <* ws
  args <- defArgParser indent <* char ':' <* eol1
  body <- bodyParser indent
  pure $ ExprDef args body

returnParser :: Indent -> Parser Expr
returnParser indent = ExprReturn <$> (string "return" *> ws1 *> exprParser indent)

-- TODO: Support this `if foo: print('reeee')`
condBlockParser :: String -> Indent -> Parser CondBlock
condBlockParser initialKeyword indent = do
  _    <- try (string initialKeyword <* ws1)
  cond <- exprParser indent <* ws <* char ':' <* eol1
  body <- bodyParser indent <* eol1
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
    elseParser =
      string indent *> string "else" *> ws *> char ':' *> eol1 *>
      bodyParser indent


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
infixParser indent = ExprInfix <$>
  exprParser' indent <* ws <*>
  infixOpParser      <* ws <*>
  exprParser indent

idParser :: Indent -> Parser Id
idParser _ = do
  x  <- char '_' <|> satisfy isAlpha
  xs <- many $ (char '_' <|> alphaNum)
  pure $ x:xs

exprIdParser :: Indent -> Parser Expr
exprIdParser indent = ExprId <$> idParser indent

intParser :: Indent -> Parser Expr
intParser _ = ExprInt . read <$> many1 digit

stringParser :: Indent -> Parser Expr
stringParser _ = do
  startQuote <- oneOf "\"'"
  inner      <- many $ noneOf [startQuote]
  _          <- char startQuote
  pure $ ExprString inner

-- TODO: Only allow in root scopes
classParser :: Indent -> Parser Expr
classParser indent = do
  _ <- try $ string "class" <* ws1
  name <- idParser indent
  -- TODO: Actually use this
  _ <- optional $ try $ multiParenParser '(' ')' (idParser indent)
  _ <- ws <* char ':' <* eol1
  defs <- blockParser indent jaja
  pure $ ExprClass name defs

  where
    jaja :: Indent -> Parser TypeCons
    jaja ind = do
      name <- idParser ind <* ws
      args <- defArgParser ind
      pure $ TypeCons name args

listParser :: Indent -> Parser Expr
listParser indent = ExprList <$> multiParenParser '[' ']' (exprParser indent)

typeParser :: Indent -> Parser Expr
typeParser indent = do
    -- TODO: Allow "type Monad(Applicative, Functor)"
    _    <- try $ string "type" <* ws1
    name <- idParser indent <* ws <* char ':' <* eol1
    body <- blockParser indent typeBodyParser
    pure $ ExprType name body
  where
    typeBodyParser :: Indent -> Parser TypeHeader
    typeBodyParser ind = do
      name <- string "def" *> ws1 *> idParser ind
      args <- multiParenParser '(' ')' (idParser ind)
      pure $ TypeHeader name args

exprParser' :: Indent -> Parser Expr
exprParser' indent = try (parenEater $ exprParser indent) <|> content
  where content = choice $ try . ($ indent) <$> [
            exprIdParser,
            intParser,
            stringParser
          ]

exprParser :: Indent -> Parser Expr
exprParser indent = choice [
    ifParser indent,
    classParser indent,
    try $ defParser indent,
    try $ namedDefParser indent,
    try $ infixParser indent,
    try $ assignmentParser indent,
    try $ exprCallParser indent,
    try $ returnParser indent,
    try $ typeParser indent,
    listParser indent,
    consParser indent,
    exprParser' indent
  ]

-- Pretty epic
blockParser :: Indent -> (Indent -> Parser a) -> Parser [a]
blockParser base parser = do
    nextIndent <- (<>) <$> string base <*> ws1
    first      <- parser nextIndent <* lookAhead singleEol
    rest       <- many $ try $ stmt nextIndent
    pure $ first:rest
  where
    stmt nextIndent = eol1 *> string nextIndent *> parser nextIndent

bodyParser :: Indent -> Parser [Expr]
bodyParser base = blockParser base exprParser

rootBodyParser :: Parser [Expr]
rootBodyParser = do
    _ <- many $ try singleEol
    first <- exprParser "" <* lookAhead singleEol
    rest  <- many $ try $ stmt
    _ <- (many $ try singleEol) <* eof
    pure $ first:rest
  where
    stmt :: Parser Expr
    stmt = eol1 *> exprParser ""
