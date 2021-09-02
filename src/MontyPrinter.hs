module MontyPrinter where

import Evaluators.Evaluatable
import Data.List (intercalate)

run :: [ET] -> IO ()
run prog = do
  putStrLn $ intercalate "\n" (render <$> prog)
