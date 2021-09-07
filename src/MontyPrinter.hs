module MontyPrinter where

import Data.List (intercalate)
import MiddleEndTypes
import PrettyPrint
import Control.Lens
import Control.Monad.State (runState)

inferMeDaddy :: Env -> MExpr  -> Env
inferMeDaddy env expr = env

run :: [MExpr] -> IO ()
run prog = do
  putStrLn $ intercalate "\n" (show <$> prog)
  print $ foldl inferMeDaddy emptyEnv prog 

  pure ()
