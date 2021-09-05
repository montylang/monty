module ModuleLoader
  ( montyParseFromFile,
    toParseExcept,
    montyRunSemantic,
  )
where

import System.Directory
import System.Exit
import System.FilePath
import Data.List
import Data.Void
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Megaparsec

import Evaluators.Evaluatable
import ParserTypes
import RunnerTypes
import RunnerUtils
import Parser.Root
import Parser.Semantic
import MiddleEndTypes

toParseExcept :: Either (ParseErrorBundle String Void) a -> ParseExcept a
toParseExcept (Right a) = pure a
toParseExcept (Left err) = throwError $ ErrParse err

montyParseFromFile :: String -> IO (ParseExcept [PExpr])
montyParseFromFile file =
  toParseExcept <$> (runParser rootBodyParser file <$> readFile file)

montyRunSemantic :: ParseExcept [PExpr] -> IO (ParseExcept [MExpr])
montyRunSemantic parsed = do
  pure $ sequence =<< (semantic <$>) <$> parsed
