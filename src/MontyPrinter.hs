module MontyPrinter where

import Evaluators.Evaluatable
import Data.List (intercalate)
import MiddleEndTypes

run :: [MExpr] -> IO ()
run prog = do
  putStrLn $ intercalate "\n" (show <$> prog)
