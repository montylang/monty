module MontyPrinter where

import Evaluators.Evaluatable
import Data.List (intercalate)
import MiddleEndTypes
import PrettyPrint

run :: [MExpr] -> IO ()
run prog = do
  putStrLn $ intercalate "\n" (prettyPrint <$> prog)
