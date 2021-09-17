module Parser.TypeParser where

import ParserTypes (Parser)
import Control.Applicative hiding (some, many)
import Text.Megaparsec
import Text.Megaparsec.Char
import MiddleEndTypes (MType (..))
import Parser.Utils

parseVar :: Parser MType
parseVar = MVar <$> some lowerChar

parseBool :: Parser MType
parseBool = MBool <$ string "Bool"

parseInt :: Parser MType
parseInt = MInt <$ string "Int"

parseFun :: Parser MType
parseFun = do
  x:xs <- sepBy1 parseSig' (ws *> string "->" <* ws)
  pure $ foldl MFun x xs

parseSig' :: Parser MType
parseSig' = choice [parseVar, parseBool, parseInt]

parseSig :: Parser MType
parseSig = choice [try parseFun, parseSig']
