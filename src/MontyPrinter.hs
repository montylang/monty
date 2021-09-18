{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module MontyPrinter where

import MyPrelude

import qualified Data.HashMap.Strict as HM
import MiddleEndTypes
import Inference
import InferenceTypes

run :: [ExistsMExpr] -> IO ()
run exprs = do
  let (res, tiState) = runTI (inferTopLevelDefs exprs)
  case res of
    Left err -> putStrLn $ show exprs <> "\nerror: " <> err
    Right _  -> do
        let defs = HM.toList $ tiState ^. globalDefs 
        traverse_ (uncurry p) defs
  where
    p :: String -> MType -> IO ()
    p name t = putStrLn $ name <> " :: " <> show t
