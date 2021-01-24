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
    idArgParser = IdArg <$> varIdParser

    intArgParser :: Parser Arg
    intArgParser = IntArg <$> intParser

    charArgParser :: Parser Arg
    charArgParser = CharArg <$> charParser

    stringArgParser :: Parser Arg
    stringArgParser = consFolder . (CharArg <$>) <$> stringParser

    patternArgParser :: Parser Arg
    patternArgParser = do
      name <- typeIdParser <* ws
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
typeConsArgParser indent = multiParenParser '(' ')' varIdParser <* ws

varIdParser :: Parser Id
varIdParser = do
  x  <- char '_' <|> satisfy isLower
  xs <- many $ (char '_' <|> alphaNumChar)
  pure $ x:xs

typeIdParser :: Parser Id
typeIdParser = do
  firstChar <- satisfy isUpper
  rest      <- many $ (char '_' <|> alphaNumChar)
  pure $ firstChar:rest

anyIdParser :: Parser Id
anyIdParser = try varIdParser <|> typeIdParser
