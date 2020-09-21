module MontyParser where

import Data.Char
import Data.Maybe
import Debug.Trace
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char

import ParserTypes

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar <* ws

ws :: Parser String
ws = many $ char ' '

ws1 :: Parser String
ws1 = some $ char ' '

commentEater :: Parser ()
commentEater = char '#' *> many (noneOf "\n") *> pure ()

-- TODO: Support CRLF and ;
singleEol :: Parser ()
singleEol = ws *> optional commentEater *> char '\n' *> pure ()

eol1 :: Parser ()
eol1 = singleEol <* (many $ try singleEol)

moduleParser :: Parser String
moduleParser = some $ alphaNumChar <|> char '.'

addPos :: a -> Parser (Pos a)
addPos expr = do
  pos <- getSourcePos
  pure $ Pos pos expr

assignmentParser :: Indent -> Parser PExpr
assignmentParser indent = do
  var  <- try $ (varIdParser indent <* ws <* char '=' <* ws)
  expr <- exprParser indent
  addPos $ ExprAssignment var expr

argParser :: Indent -> Parser Arg
argParser indent = choice $ try <$> [
    consArgParser, -- Order matters
    patternArgParser,
    idArgParser
  ]
  where
    idArgParser :: Parser Arg
    idArgParser = IdArg <$> varIdParser indent

    patternArgParser :: Parser Arg
    patternArgParser = do
      name <- typeIdParser indent <* ws
      args <- try (defArgParser indent) <|> pure []
      pure $ PatternArg name args

    consArgParser :: Parser Arg
    consArgParser = do
      headArg <- try $ idArgParser <* ws <* char '|' <* ws
      tailArg <- try idArgParser <|> try consArgParser
      pure $ PatternArg "Cons" [headArg, tailArg]

defArgParser :: Indent -> Parser [Arg]
defArgParser indent = multiParenParser '(' ')' (argParser indent) <* ws

typeConsArgParser :: Indent -> Parser [Id]
typeConsArgParser indent = multiParenParser '(' ')' (varIdParser indent) <* ws

namedDefParser :: Indent -> Parser PExpr
namedDefParser indent = do
  name <- try $ string "def" *> ws1 *> varIdParser indent
  args <- defArgParser indent <* char ':' <* eol1
  body <- bodyParser indent
  def  <- addPos $ ExprDef args body
  addPos $ ExprAssignment name def

consParser :: Indent -> Parser PExpr
consParser indent = do
  headExpr <- try $ exprParser' indent <* ws <* char '|' <* ws
  tailExpr <- exprParser indent
  exprId   <- addPos $ ExprId "Cons"
  addPos $ ExprCall exprId [headExpr, tailExpr]

-- Supports f() and f()()
exprCallParser :: Indent -> Parser PExpr
exprCallParser indent = do
    fun           <- try (exprParser' indent <* ws <* (lookAhead $ char '('))
    firstArgLists <- multiParenParser '(' ')' $ exprParser indent
    otherArgLists <- many $ multiParenParser '(' ')' $ exprParser indent
    call          <- addPos $ ExprCall fun firstArgLists
    pure $ foldl q call otherArgLists
  where
    q :: PExpr -> [PExpr] -> PExpr
    q pos args = extend (flip ExprCall args) pos
    
    extend :: (Pos a -> b) -> Pos a -> Pos b 
    extend fun position@(Pos pos _) =
      Pos pos (fun position)

-- Bit of a hack but so is this entire language so whatever
emptyTypeCallParser :: Indent -> Parser PExpr
emptyTypeCallParser indent = do
  fun    <- typeIdParser indent
  exprId <- addPos $ ExprId fun
  addPos $ ExprCall exprId []

-- Eats surrounding parens of an expr, for disambiguation
parenEater :: Parser PExpr -> Parser PExpr
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

defParser :: Indent -> Parser PExpr
defParser indent = do
  args <- try $ rword "def" *> defArgParser indent <* char ':' <* eol1
  body <- bodyParser indent
  addPos $ ExprDef args body

lambdaParser :: Indent -> Parser PExpr
lambdaParser indent = do
  args <- try $ defArgParser indent <* char ':' <* ws
  body <- exprParser indent
  ret  <- addPos $ ExprReturn body
  addPos $ ExprDef args [ret]

returnParser :: Indent -> Parser PExpr
returnParser indent = try (rword "return") *>
  (ExprReturn <$> (exprParser indent) >>= addPos)

-- TODO: Support this `if foo: print('reeee')`
condBlockParser :: String -> Indent -> Parser CondBlock
condBlockParser initialKeyword indent = do
  cond <- try $ rword initialKeyword *> exprParser indent <* ws <* char ':' <* eol1
  body <- bodyParser indent <* eol1
  pure $ CondBlock cond body

ifParser :: Indent -> Parser PExpr
ifParser indent = do
    fi   <- condBlockParser "if" indent 
    file <- elifsParser
    esle <- elseParser
    addPos $ ExprIfElse fi file esle
  where
    elifsParser :: Parser [CondBlock]
    elifsParser = many (try $ condBlockParser "elif" indent)

    elseParser :: Parser [PExpr]
    elseParser =
      string indent *> rword "else" *> char ':' *> eol1 *>
      bodyParser indent

infixOpParser :: Parser PExpr -> Parser (PExpr -> Expr)
infixOpParser lhsParser = do
    lhs <- lhsParser <* ws
    op  <- (choice $ opParser <$> arr) <* ws
    pure $ ExprInfix lhs op
  where
    opParser :: (String, InfixOp) -> Parser InfixOp
    opParser (s, o) = string s *> pure o
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

infixParser :: Indent -> Parser PExpr
infixParser indent = do
  first  <- try $ infixOpParser (exprParser' indent)
  second <- exprParser indent
  addPos $ first second

varIdParser :: Indent -> Parser Id
varIdParser _ = do
  x  <- char '_' <|> satisfy isLower
  xs <- many $ (char '_' <|> alphaNumChar)
  pure $ x:xs

typeIdParser :: Indent -> Parser Id
typeIdParser _ = do
  firstChar <- satisfy isUpper
  rest      <- many $ (char '_' <|> alphaNumChar)
  pure $ firstChar:rest

anyIdParser :: Indent -> Parser Id
anyIdParser indent = try (varIdParser indent) <|> typeIdParser indent

exprIdParser :: Indent -> Parser PExpr
exprIdParser indent = ExprId <$> anyIdParser indent >>= addPos

intParser :: Indent -> Parser PExpr
intParser _ = ExprInt . read <$> some digitChar >>= addPos

stringParser :: Indent -> Parser PExpr
stringParser _ = do
  startQuote <- oneOf "\"'"
  inner      <- many $ noneOf [startQuote]
  _          <- char startQuote
  addPos $ ExprString inner

-- TODO: Only allow in root scopes
classParser :: Indent -> Parser PExpr
classParser indent = do
  _ <- try $ string "class" <* ws1
  name <- typeIdParser indent
  _ <- ws <* char ':' <* eol1
  defs <- blockParser indent typeConsParser
  addPos $ ExprClass name defs

  where
    typeConsParser :: Indent -> Parser (Pos TypeCons)
    typeConsParser ind = do
      name <- typeIdParser ind <* ws
      args <- (try $ typeConsArgParser ind) <|> pure []
      addPos $ TypeCons name args

instanceParser :: Indent -> Parser PExpr
instanceParser indent = do
    name        <- try $ string "instance" *> ws1 *> typeIdParser indent
    _           <- ws1 <* string "of" <* ws1
    typeClass   <- typeIdParser indent <* ws <* char ':' <* eol1
    definitions <- blockParser indent namedDefParser
    addPos $ ExprInstanceOf name typeClass definitions

typeParser :: Indent -> Parser PExpr
typeParser indent = do
    _    <- try $ string "type" <* ws1
    name <- typeIdParser indent <* ws <* char ':' <* eol1
    body <- blockParser indent $ typeBodyParser name
    addPos $ ExprType name body
  where
    typeBodyParser :: Id -> Indent -> Parser (Pos DefSignature)
    typeBodyParser typeName ind = do
      name <- string "def" *> ws1 *> varIdParser ind
      args <- multiParenParser '(' ')' (varIdParser ind)
      addPos $ DefSignature typeName name (nameToArg <$> args)

    nameToArg :: Id -> Arg
    nameToArg "self" = SelfArg
    nameToArg  name  = IdArg name

listParser :: Indent -> Parser PExpr
listParser indent =
  ExprList <$> multiParenParser '[' ']' (exprParser indent) >>= addPos

exprParser' :: Indent -> Parser PExpr
exprParser' indent = try (parenEater $ exprParser indent) <|> content
  where
    content = choice $ ($ indent) <$> [
        try . exprIdParser,
        intParser,
        stringParser
      ]

exprParser :: Indent -> Parser PExpr
exprParser indent = choice $ ($ indent) <$> [
    returnParser,
    classParser,
    ifParser,
    namedDefParser,
    defParser,
    lambdaParser,
    infixParser,
    assignmentParser,
    exprCallParser,
    try . emptyTypeCallParser,
    typeParser,
    instanceParser,
    listParser,
    consParser,
    exprParser'
  ]

-- Pretty epic
blockParser :: Indent -> (Indent -> Parser a) -> Parser [a]
blockParser base parser = do
    nextIndent <- (<>) <$> string base <*> ws1
    first      <- parser nextIndent <* lookAhead (singleEol <|> eof)
    rest       <- many $ stmt nextIndent
    pure $ first:rest
  where
    stmt nextIndent = (try $ eol1 *> string nextIndent) *> parser nextIndent

bodyParser :: Indent -> Parser [PExpr]
bodyParser base = blockParser base exprParser

rootBodyParser :: Parser [PExpr]
rootBodyParser = do
    _ <- many $ try singleEol
    first <- exprParser "" <* lookAhead singleEol
    rest  <- many stmt <* eof
    pure $ first:(catMaybes rest)
  where
    stmt :: Parser (Maybe PExpr)
    stmt = try blankLine <|> something
      where
        blankLine :: Parser (Maybe PExpr)
        blankLine = eol1 *> pure Nothing

        something :: Parser (Maybe PExpr)
        something = Just <$> exprParser ""
