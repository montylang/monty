module MyDoc.Generator where

import Data.Void
import Text.Megaparsec

type DocParser = Parsec Void String

data ParamId
  = ParamIndex Int
  | ParamName String

data Type
  = TypeList Type
  | TypeTuple [Type]
  | TypeFunc [Type] Type

data Param = Param ParamId Type
