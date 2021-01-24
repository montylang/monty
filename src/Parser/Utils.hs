module Parser.Utils where

import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import ParserTypes
import Control.Monad (liftM)

rword :: String -> Parser ()
rword w = try (string w *> notFollowedBy alphaNumChar) <* ws

sc :: Parser ()
sc = () <$ ws

isHsWs :: Char -> Bool
isHsWs ' ' = True
isHsWs '\t' = True
isHsWs _    = False

ws :: Parser String
ws = takeWhileP Nothing isHsWs

ws1 :: Parser String
ws1 = takeWhile1P Nothing isHsWs

commentEater :: Parser ()
commentEater = char '#' *> takeWhileP Nothing (/= '\n') *> pure ()

-- TODO: Support CRLF and ;
singleEol :: Parser ()
singleEol = ws *> optional commentEater *> char '\n' *> pure ()

commentableEof :: Parser ()
commentableEof = () <$ many (try singleEol) <*
  (ws *> optional commentEater *> eof)

eolMany :: Parser ()
eolMany = pure () <* (many $ try singleEol)

eolSome :: Parser ()
eolSome = pure () <* (some $ try singleEol)

sourcePos :: Parser SourcePos
sourcePos = getSourcePos
-- TODO: This is faster apparently, but it's broken
--sourcePos = pstateSourcePos <$> liftM statePosState getParserState

addPos :: a -> Parser (Pos a)
addPos expr = do
  pos <- sourcePos
  pure $ Pos pos expr

-- Eats surrounding parens of an expr, for disambiguation
precedenceP :: Parser PExpr -> Parser PExpr
precedenceP innerParser = do
    inner <- char '(' *> delimWs *> innerParser <* delimWs <* char ')'
    addPos $ ExprPrecedence inner
  where
    delimWs = eolMany *> ws

-- Matches syntax of the form (anything, anything, ...)
multiParenParser :: Char -> Char -> Parser a -> Parser [a]
multiParenParser open close innerParser =
    char open *> delimWs *>
    sepBy (delimWs *> innerParser <* delimWs) (char ',')
    <* delimWs <* char close
  where
    delimWs = eolMany *> ws

-- Pretty epic
blockParser :: Indent -> (Indent -> Parser a) -> Parser [a]
blockParser base parser = do
    nextIndent <- (<>) <$> string base <*> ws1
    first      <- parser nextIndent <* lookAhead (singleEol <|> eof)
    rest       <- many $ stmt nextIndent
    pure $ first:rest
  where
    stmt nextIndent = (try $ eolSome *> string nextIndent) *> parser nextIndent
