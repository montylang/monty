module Parser.Root where

import Debug.Trace
import Data.Char
import Data.Maybe
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Lens.Micro.Platform

import ParserTypes
import MorphUtils

import Parser.Literal
import Parser.Utils
import Parser.Arg

moduleParser :: Parser String
moduleParser = some $ alphaNumChar <|> char '.'

passParser :: Indent -> Parser PExpr
passParser indent = do
  _    <- try $ rword "pass"
  name <- addPos $ ExprId "pass"
  addPos $ ExprCall name []

assignmentParser :: Indent -> Parser PExpr
assignmentParser indent = do
  dest <- try $ (argParser indent <* ws <* char '=' <* ws)
  expr <- exprParser indent
  addPos $ ExprAssignment dest expr

namedDefParser :: Indent -> Parser PExpr
namedDefParser indent = do
  name <- try $ string "def" *> ws1 *> varIdParser indent
  args <- defArgParser indent <* char ':' <* eolSome
  body <- bodyParser indent
  def  <- addPos $ ExprDef args body
  addPos $ ExprAssignment (IdArg name) def

consParser :: Indent -> Parser PExpr
consParser indent = do
  headExpr <- try $ exprParser' indent <* ws <* char '|' <* ws
  tailExpr <- exprParser indent
  exprId   <- addPos $ ExprId "Cons"
  addPos $ ExprCall exprId [headExpr, tailExpr]

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
returnParser indent = try (rword "return") *>
  (ExprReturn <$> (exprParser indent) >>= addPos)

caseParser :: Indent -> Parser PExpr
caseParser indent = do
  pos    <- getSourcePos
  _      <- try $ rword "case"
  cond   <- exprParser indent <* ws <* char ':' <* eolSome
  blocks <- blockParser indent caseBlockParser
  pure $ Pos pos $ ExprCase cond blocks

caseBlockParser :: Indent -> Parser (Pos CaseBlock)
caseBlockParser indent = do
    pos  <- getSourcePos
    cond <- try $ argParser indent <* ws <* char ':'
    body <- (try eolSome *> multiLineP) <|> oneLineP
    pure $ Pos pos $ CaseBlock cond body
  where
    oneLineP :: Parser [PExpr]
    oneLineP = pure <$> (ws *> exprParser indent)
    
    multiLineP :: Parser [PExpr]
    multiLineP = blockParser indent exprParser

-- TODO: Support this `if foo: print('reeee')`
condBlockParser :: String -> Indent -> Parser CondBlock
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
    elifsParser :: Parser [CondBlock]
    elifsParser = many (try $ condBlockParser "elif" indent)

    elseParser :: Parser [PExpr]
    elseParser =
      string indent *> rword "else" *> char ':' *> eolSome *>
      bodyParser indent

infixParser :: Indent -> Parser PExpr
infixParser indent = do
    first <- try $ exprParser' indent <* ws <* (lookAhead allOpParser)
    rest  <- many partialOpParser
    pure $ groupByPrecedence (((view _2) <$> arr)) ((Nothing, first):rest)
  where
    arr = [
        ("<>", InfixMappend),
        ("+", InfixAdd),
        ("-", InfixSub),
        ("*", InfixMul),
        ("/", InfixDiv),
        ("%", InfixMod),
        ("!=", InfixNe),
        ("==", InfixEq),
        ("is", InfixEq),
        ("<=", InfixLe),
        (">=", InfixGe),
        (">", InfixGt),
        ("<", InfixLt),
        ("and", InfixLogicAnd),
        ("or", InfixLogicOr)
      ]

    -- What a mess
    groupByPrecedence :: [InfixOp] -> [(Maybe InfixOp, PExpr)] -> PExpr
    groupByPrecedence [] [] = trace "How did I get here?" undefined
    groupByPrecedence [] [(_, x)] = x
    groupByPrecedence [] ((Just op, (Pos p x)):xs) =
      -- TODO: Nothing about this is ok
      Pos p $ ExprInfix (Pos p x) op $ groupByPrecedence [] xs
    groupByPrecedence (o:os) xs  = joinHeadOp subCases
      where
        subCases :: [PExpr]
        subCases = groupByPrecedence os <$>
          (multiSpan ((== (Just o)) . (view _1)) xs)

        joinHeadOp :: [PExpr] -> PExpr
        joinHeadOp [y] = y
        joinHeadOp (y:ys) = foldl folderHeadOp y ys

        folderHeadOp :: PExpr -> PExpr -> PExpr
        folderHeadOp acc it = Pos (getPos it) $ ExprInfix acc o it

    opParser :: (String, InfixOp) -> Parser InfixOp
    opParser (s, o) = string s *> pure o

    allOpParser = choice $ opParser <$> arr

    partialOpParser :: Parser (Maybe InfixOp, PExpr)
    partialOpParser = do
      op  <- try $ ws *> allOpParser <* ws
      rhs <- exprParser' indent <* ws
      pure (Just op, rhs)

exprVarIdParser :: Indent -> Parser PExpr
exprVarIdParser indent = ExprId <$> varIdParser indent >>= addPos

exprTypeIdParser :: Indent -> Parser PExpr
exprTypeIdParser indent = ExprId <$> typeIdParser indent >>= addPos

exprTypeConsParser :: Indent -> Parser PExpr
exprTypeConsParser indent = do
  name <- ExprId <$> typeIdParser indent >>= addPos
  args <- (multiParenParser '(' ')' $ exprParser indent) <|> pure []
  addPos $ ExprCall name args

exprIntParser :: Indent -> Parser PExpr
exprIntParser _ = ExprInt <$> intParser >>= addPos

exprCharParser :: Indent -> Parser PExpr
exprCharParser _ = ExprChar <$> charParser >>= addPos

exprStringParser :: Indent -> Parser PExpr
exprStringParser _ = do
  pos   <- getSourcePos
  inner <- stringParser
  addPos $ ExprList (Pos pos . ExprChar <$> inner)

-- TODO: Only allow in root scopes
classParser :: Indent -> Parser PExpr
classParser indent = do
  _ <- try $ string "class" <* ws1
  name <- typeIdParser indent
  _ <- ws <* char ':' <* eolSome
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
    typeClass   <- typeIdParser indent <* ws <* char ':' <* eolSome
    definitions <- blockParser indent namedDefParser
    addPos $ ExprInstanceOf name typeClass definitions

typeParser :: Indent -> Parser PExpr
typeParser indent = do
    _    <- try $ string "type" <* ws1
    name <- typeIdParser indent <* ws <* char ':' <* eolSome
    body <- blockParser indent $ typeBodyParser name
    addPos $ ExprType name body
  where
    typeBodyParser :: Id -> Indent -> Parser (Pos DefSignature)
    typeBodyParser typeName ind = do
      name    <- string "def" *> ws1 *> varIdParser ind
      args    <- multiParenParser '(' ')' (varIdParser ind)
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
    wrappableParser ind = bindParser ind <|> exprParser' ind

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
chainableParser indent previous =
    sugarCallParser previous <|> normalCallParser previous <|> pure previous
  where
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

exprParser' :: Indent -> Parser PExpr
exprParser' indent = chainableParser indent =<< 
    (try (parenEater $ exprParser indent) <|> content)
  where
    content = choice $ ($ indent) <$> [
        passParser,
        instanceParser,
        unwrapParser,
        returnParser,
        classParser,
        caseParser,
        namedDefParser,
        defParser,
        ifParser,
        typeParser,
        exprVarIdParser,
        exprTypeConsParser,
        exprIntParser,
        listParser,
        exprCharParser,
        exprStringParser
      ]

exprParser :: Indent -> Parser PExpr
exprParser indent = choice $ ($ indent) <$> [
    lambdaParser,
    infixParser,
    assignmentParser,
    consParser,
    exprParser',
    tupleParser
  ]

importParser :: Parser PExpr
importParser = do
  _    <- try (rword "import")
  path <- sepBy1 (varIdParser "") (char '.')
  addPos $ ExprImport path

bodyParser :: Indent -> Parser [PExpr]
bodyParser base = blockParser base exprParser

rootBodyParser :: Parser [PExpr]
rootBodyParser = do
    _ <- many $ try singleEol
    imports <- many (importParser <* eolSome)
    _ <- many $ try singleEol
    first <- exprParser "" <* lookAhead (try singleEol <|> eof)
    rest  <- many stmt <* commentableEof
    pure $ imports <> (first:(catMaybes rest))
  where
    stmt :: Parser (Maybe PExpr)
    stmt = try blankLine <|> rootExpr

    blankLine :: Parser (Maybe PExpr)
    blankLine = eolSome *> pure Nothing

    rootExpr :: Parser (Maybe PExpr)
    rootExpr = Just <$> exprParser ""
