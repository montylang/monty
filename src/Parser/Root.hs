module Parser.Root where

import Debug.Trace
import Data.Char
import Data.Maybe
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Lens

import ParserTypes
import MorphUtils

import Parser.Literal
import Parser.Utils
import Parser.Arg

moduleParser :: Parser String
moduleParser = some $ alphaNumChar <|> char '.'

passParser :: Indent -> Parser PExpr
passParser indent = do
  _    <- rword "pass"
  name <- addPos $ ExprId "pass"
  addPos $ ExprCall name []

assignmentParser :: Indent -> Parser PExpr
assignmentParser indent = do
  dest <- try $ (
      argParser indent <* ws <* char '=' <*
      lookAhead (try $ anySingleBut '=') <* ws
    )
  expr <- exprParser indent
  addPos $ ExprAssignment dest expr

namedDefParser :: Indent -> Parser PExpr
namedDefParser indent = do
  name <- try $ string "def" *> ws1 *> varIdParser
  args <- defArgParser indent <* char ':' <* eolSome
  body <- bodyParser indent
  def  <- addPos $ ExprDef args body
  addPos $ ExprAssignment (IdArg name) def

defParser :: Indent -> Parser PExpr
defParser indent = do
  args <- try $ rword "def" *> defArgParser indent <* char ':' <* eolSome
  body <- bodyParser indent
  addPos $ ExprDef args body

lambdaParser :: Indent -> Parser PExpr
lambdaParser indent = do
  args <- try $ defArgParser indent <* char ':' <* ws
  body <- exprParser indent
  ret  <- addPos $ ExprReturn body
  addPos $ ExprDef args [ret]

returnParser :: Indent -> Parser PExpr
returnParser indent = rword "return" *>
  (ExprReturn <$> (exprParser indent) >>= addPos)

caseParser :: Indent -> Parser PExpr
caseParser indent = do
  pos    <- sourcePos
  _      <- rword "case"
  cond   <- exprParser indent <* ws <* char ':' <* eolSome
  blocks <- blockParser indent caseBlockParser
  pure $ Pos pos $ ExprCase cond blocks

caseBlockParser :: Indent -> Parser (CaseBlock PExpr)
caseBlockParser indent = do
    pos  <- sourcePos
    cond <- try $ argParser indent <* ws <* char ':'
    body <- (try eolSome *> multiLineP) <|> oneLineP
    pure $ CaseBlock pos cond body
  where
    oneLineP :: Parser [PExpr]
    oneLineP = pure <$> (ws *> exprParser indent)
    
    multiLineP :: Parser [PExpr]
    multiLineP = blockParser indent exprParser

-- TODO: Support this `if foo: print('reeee')`
condBlockParser :: String -> Indent -> Parser (CondBlock PExpr)
condBlockParser initialKeyword indent = do
  cond <- try $ rword initialKeyword *> exprParser indent
          <* ws <* char ':' <* eolSome
  body <- bodyParser indent <* eolSome
  pure $ CondBlock cond body

ifParser :: Indent -> Parser PExpr
ifParser indent = do
    fi   <- condBlockParser "if" indent 
    file <- elifsParser
    esle <- elseParser
    addPos $ ExprIfElse fi file esle
  where
    elifsParser :: Parser [CondBlock PExpr]
    elifsParser = many (try $ condBlockParser "elif" indent)

    elseParser :: Parser [PExpr]
    elseParser =
      string indent *> rword "else" *> char ':' *> eolSome *>
      bodyParser indent

exprVarIdParser :: Indent -> Parser PExpr
exprVarIdParser indent = ExprId <$> varIdParser >>= addPos

exprTypeIdParser :: Indent -> Parser PExpr
exprTypeIdParser indent = ExprId <$> typeIdParser >>= addPos

exprTypeConsParser :: Indent -> Parser PExpr
exprTypeConsParser indent = do
  name <- ExprId <$> typeIdParser >>= addPos
  args <- (multiParenParser '(' ')' $ exprParser indent) <|> pure []
  addPos $ ExprCall name args

exprIntParser :: Indent -> Parser PExpr
exprIntParser _ = ExprInt <$> intParser >>= addPos

exprDoubleParser :: Indent -> Parser PExpr
exprDoubleParser _ = ExprDouble <$> doubleParser >>= addPos

exprCharParser :: Indent -> Parser PExpr
exprCharParser _ = ExprChar <$> charParser >>= addPos

exprStringParser :: Indent -> Parser PExpr
exprStringParser _ = do
  pos   <- sourcePos
  inner <- stringParser
  addPos $ ExprList (Pos pos . ExprChar <$> inner)

classParser :: Parser PExpr
classParser = do
    pos <- sourcePos
    _ <- try $ string "class" <* ws1
    name <- typeIdParser
    _ <- ws <* char ':' <* eolSome
    defs <- blockParser "" typeConsParser
    pure $ Pos pos $ ExprClass name defs
  where
    typeConsParser :: Indent -> Parser (Pos TypeCons)
    typeConsParser ind = do
      name <- typeIdParser <* ws
      args <- (try $ typeConsArgParser ind) <|> pure []
      addPos $ TypeCons name args

instanceParser :: Parser PExpr
instanceParser = do
    pos         <- sourcePos
    name        <- try $ string "instance" *> ws1 *> typeIdParser
    _           <- ws1 <* string "of" <* ws1
    typeClass   <- typeIdParser <* ws <* char ':' <* eolSome
    definitions <- blockParser "" namedDefParser
    pure $ Pos pos $ ExprInstanceOf name typeClass definitions

typeParser :: Parser PExpr
typeParser = do
    pos  <- sourcePos
    _    <- try $ string "type" <* ws1
    name <- typeIdParser <* ws <* char ':' <* eolSome
    body <- blockParser "" $ typeBodyParser name
    pure $ Pos pos $ ExprType name body
  where
    typeBodyParser :: Id -> Indent -> Parser (Pos DefSignature)
    typeBodyParser typeName ind = do
      name    <- string "def" *> ws1 *> varIdParser
      args    <- multiParenParser '(' ')' varIdParser
      retSelf <- mustReturnSelfP
      addPos $ DefSignature typeName name (nameToArg <$> args) retSelf

    mustReturnSelfP :: Parser Bool
    mustReturnSelfP = do
      ret <- optional $ ws *> string "->" *> ws *> string "self"
      pure (maybe False (== "self") ret)

    nameToArg :: Id -> Arg
    nameToArg "self" = SelfArg
    nameToArg  name  = IdArg name

unwrapParser :: Indent -> Parser PExpr
unwrapParser indent = do
    _       <- try $ string "unwrap" <* ws <* char ':' <* eolSome
    content <- blockParser indent wrappableParser
    addPos $ ExprUnwrap content
  where
    wrappableParser :: Indent -> Parser PExpr
    wrappableParser ind = bindParser ind <|> exprParser ind

    bindParser :: Indent -> Parser PExpr
    bindParser ind = do
      arg   <- try $ argParser ind <* ws <* string "<-" <* ws
      value <- exprParser ind
      addPos $ ExprBind arg value

listParser :: Indent -> Parser PExpr
listParser indent =
  ExprList <$> multiParenParser '[' ']' (exprParser indent) >>= addPos

tupleParser :: Indent -> Parser PExpr
tupleParser indent = try $ do
  tup <- ExprTuple <$> multiParenParser '(' ')' (exprParser indent)
  addPos tup

chainableParser :: Indent -> PExpr -> Parser PExpr
chainableParser indent previous = do
    mayC <- fancyLookAhead

    case mayC of
      Nothing -> pure previous
      Just c  -> choosy c <|> pure previous
  where
    fancyLookAhead :: Parser (Maybe Char)
    fancyLookAhead =
      lookAhead $ eolMany *> ws *>
      ((Just <$> anySingle) <|> (eof *> pure Nothing))

    choosy :: Char -> Parser PExpr
    choosy c = case c of
      '.' -> sugarCallParser previous
      '(' -> normalCallParser previous
      '|' -> consParser previous
      '<' -> choice [
          infixP "<>" InfixMappend previous,
          infixP "<=" InfixLe previous,
          infixP "<"  InfixLt previous
        ]
      '>' -> choice [
          infixP ">=" InfixGe previous,
          infixP ">"  InfixGt previous
        ]
      '+' -> infixP "+" InfixAdd previous
      '-' -> infixP "-" InfixSub previous
      '*' -> infixP "*" InfixMul previous
      '/' -> infixP "/" InfixDiv previous
      '%' -> infixP "%" InfixMod previous
      '!' -> infixP "!=" InfixNe previous
      '=' -> infixP "==" InfixEq previous
      'i' -> infixP "is" InfixEq previous
      'a' -> infixP "and" InfixLogicAnd previous
      'o' -> infixP "or" InfixLogicOr previous
      _   -> empty

    infixP :: String -> InfixOp -> PExpr -> Parser PExpr
    infixP opString op lhs = do
      op  <- try $ ws *> string opString *> pure op <* ws
      rhs <- exprParser' indent
      addPos $ ExprInfix lhs op rhs

    sugarCallParser :: PExpr -> Parser PExpr
    sugarCallParser prev = do
      _       <- try $ eolMany *> ws *> char '.'
      fun     <- exprVarIdParser indent
      argList <- multiParenParser '(' ')' $ exprParser indent
      final   <- addPos $ ExprCall fun (prev:argList)
      chainableParser indent final

    normalCallParser :: PExpr -> Parser PExpr 
    normalCallParser prev = do
      argList <- multiParenParser '(' ')' $ exprParser indent
      final   <- addPos $ ExprCall prev argList
      chainableParser indent final

    consParser :: PExpr -> Parser PExpr
    consParser prev = do
      _        <- char '|' <* ws
      tailExpr <- exprParser indent
      exprId   <- addPos $ ExprId "Cons"
      addPos $ ExprCall exprId [prev, tailExpr]

prefixNot :: Indent -> Parser PExpr
prefixNot indent = do
  _  <- rword "not"
  ex <- exprParser' indent
  addPos $ ExprPrefixOp PrefixNot ex

prefixNegate :: Indent -> Parser PExpr
prefixNegate indent = do
  ex <- try $ char '-' *> exprParser' indent
  addPos $ ExprPrefixOp PrefixNegate ex

exprParser' :: Indent -> Parser PExpr
exprParser' indent = unchainableContent <|> contentChained
  where
    contentChained :: Parser PExpr
    contentChained = chainableParser indent =<<
      try (precedenceP $ exprParser indent) <|> unambigousContent

    unchainableContent :: Parser PExpr
    unchainableContent = do
      c <- lookAhead anySingle

      case c of
        'i' -> ifParser indent
        'd' -> choice $ ($ indent) <$> [
                 namedDefParser,
                 defParser
               ]
        'c' -> caseParser indent
        'u' -> unwrapParser indent
        'r' -> returnParser indent
        'p' -> passParser indent
        _   -> empty

    unambigousContent :: Parser PExpr
    unambigousContent = do
      c <- lookAhead anySingle

      case c of
        '[' -> listParser indent
        '\''-> exprCharParser indent
        '"' -> exprStringParser indent
        '-' -> choice $ ($ indent) <$> [
                 exprDoubleParser,
                 exprIntParser,
                 prefixNegate
               ]
        a | isDigit a -> choice $ ($ indent) <$> [
                 exprDoubleParser,
                 exprIntParser
               ]
        a | isUpper a -> exprTypeConsParser indent
        a | isLower a -> choice $ ($ indent) <$> [
                 prefixNot,
                 exprVarIdParser
               ]
        _ -> empty

exprParser :: Indent -> Parser PExpr
exprParser indent = choice $ ($ indent) <$> [
    lambdaParser,
    assignmentParser,
    exprParser',
    tupleParser
  ]

importParser :: Parser PExpr
importParser = do
  _ <- rword "import"
  path <- sepBy1 varIdParser (char '.')
  addPos $ ExprImport path

statementParser :: Parser PExpr
statementParser = do
    c <- lookAhead anySingle

    case c of
      'i' -> instanceParser
      'c' -> classParser
      't' -> typeParser
      _   -> empty

bodyParser :: Indent -> Parser [PExpr]
bodyParser base = blockParser base exprParser

rootPeep :: Parser PExpr
rootPeep = statementParser <|> exprParser ""

rootBodyParser :: Parser [PExpr]
rootBodyParser = do
    _ <- many $ try singleEol
    imports <- many (importParser <* eolSome)
    _ <- many $ try singleEol
    first <- rootPeep <* lookAhead (try singleEol <|> eof)
    rest  <- many stmt <* commentableEof
    pure $ imports <> (first:(catMaybes rest))
  where
    stmt :: Parser (Maybe PExpr)
    stmt = try blankLine <|> rootExpr

    blankLine :: Parser (Maybe PExpr)
    blankLine = eolSome *> pure Nothing

    rootExpr :: Parser (Maybe PExpr)
    rootExpr = Just <$> rootPeep
