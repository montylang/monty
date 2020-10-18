module Parser.Arg where

import Data.Char
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Parser.Literal
import Parser.Utils
import ParserTypes

argParser :: Indent -> Parser Arg
argParser indent = choice $ try <$> [
    consArgParser,
    tupleArgParser,
    listArgParser,
    intArgParser,
    charArgParser,
    stringArgParser,
    patternArgParser,
    idArgParser
  ]
  where
    idArgParser :: Parser Arg
    idArgParser = IdArg <$> varIdParser indent

    intArgParser :: Parser Arg
    intArgParser = IntArg <$> intParser

    charArgParser :: Parser Arg
    charArgParser = CharArg <$> charParser

    stringArgParser :: Parser Arg
    stringArgParser = consFolder <$> (CharArg <$>) <$> stringParser

    patternArgParser :: Parser Arg
    patternArgParser = do
      name <- typeIdParser indent <* ws
      args <- try (defArgParser indent) <|> pure []
      pure $ PatternArg name args

    tupleArgParser :: Parser Arg
    tupleArgParser = do
      args <- multiParenParser '(' ')' (argParser indent)
      pure $ PatternArg "Tuple" args

    listArgParser :: Parser Arg
    listArgParser =
      consFolder <$> multiParenParser '[' ']' (argParser indent)

    consFolder :: [Arg] -> Arg
    consFolder []     = PatternArg "Nil" []
    consFolder (x:xs) = PatternArg "Cons" [x, consFolder xs]

    consArgParser :: Parser Arg
    consArgParser = do
      headArg <- try $ idArgParser <* ws <* char '|' <* ws
      tailArg <- try idArgParser <|> try consArgParser
      pure $ PatternArg "Cons" [headArg, tailArg]

defArgParser :: Indent -> Parser [Arg]
defArgParser indent = multiParenParser '(' ')' (argParser indent) <* ws

typeConsArgParser :: Indent -> Parser [Id]
typeConsArgParser indent = multiParenParser '(' ')' (varIdParser indent) <* ws

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
