module Parser.Utils where

import Data.Char
import Control.Lens
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import ParserTypes
import Control.Monad (liftM)
import Debug.Trace

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

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace 

commentEater :: Parser ()
commentEater = do
    comment <- char '#' *> takeWhileP Nothing (/= '\n')
    currentDocString %= thing comment
  where
    thing :: DocString -> [DocString] -> [DocString]
    thing new current = trace "o no" $ current <> filter (\s -> take 1 s == "|") [new]

-- Cannot be at the end of a non-empty line
docstringEater :: Parser ()
docstringEater = trace "Try parsing a docstring you ass hate" $ do
  content <- some (try thing)
  trace (show content) currentDocString .= content
  where
    thing = ws *> string "#|" *> takeWhileP Nothing (/= '\n') <* optional (char '\n')

-- TODO: Support CRLF and ;
singleEol :: Parser ()
singleEol = trace "singleton object eol" $ () <$ (ws *> optional commentEater *> char '\n')

commentableEof :: Parser ()
commentableEof = () <$ many (try singleEol) <*
  (ws *> optional commentEater *> eof)

eolMany :: Parser ()
eolMany = do
  () <$ many (try singleEol)

  -- if null parsed
  --   then pure ()
  --   else currentDocString .= []

eolSome :: Parser ()
eolSome = do
  () <$ some (try singleEol)

  -- if null parsed
  --   then pure ()
  --   else currentDocString .= []

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
