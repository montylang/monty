{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module MontyPrinter where

import MyPrelude

import qualified Data.HashMap.Strict as HM
import MiddleEndTypes
import Inference

run :: [ExistsMExpr] -> IO ()
run exprs = do
  let (res, _) = runTI (inferMExprs (TypeEnv HM.empty) exprs)
  case res of
    Left err    -> putStrLn $ show exprs ++ "\nerror: " ++ err
    Right types -> traverse_ (uncurry p) (zip exprs types)
  where
    p :: ExistsMExpr -> MType -> IO ()
    p e t = putStrLn $ show e ++ "\n:: " ++ show t
