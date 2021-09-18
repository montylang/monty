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
  -- FIXME: Can't this be a fold?
  joinFuns <$> sepBy1 parseSig' (try $ ws *> string "->" <* ws)
  where
    joinFuns :: [MType] -> MType
    joinFuns []     = undefined
    joinFuns [x]    = x
    joinFuns (x:xs) = MFun x (joinFuns xs)

parsePar :: Parser MType
parsePar = char '(' *> ws *> parseSig <* ws <* char ')'

parseSig' :: Parser MType
parseSig' = choice [parsePar, parseVar, parseBool, parseInt]

parseSig :: Parser MType
parseSig = choice [try parseFun, parseSig']
