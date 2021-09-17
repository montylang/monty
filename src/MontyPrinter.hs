{-# LANGUAGE DataKinds #-}
module MontyPrinter where

import MyPrelude

import qualified Data.HashMap.Strict as HM
import MiddleEndTypes
import Inference

run :: [ExistsMExpr] -> IO ()
run exprs = do
  let (res, _) = runTI (inferMExprs (TypeEnv HM.empty) exprs)
  case res of
    Left err -> putStrLn $ show exprs ++ "\nerror: " ++ err
    Right t  -> putStrLn $ show exprs ++ "\n:: " ++ show t
