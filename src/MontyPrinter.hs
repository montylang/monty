{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module MontyPrinter where

import MyPrelude

import qualified Data.HashMap.Strict as HM
import MiddleEndTypes
import Inference
import InferenceTypes
import qualified Data.HashSet.Internal as HS

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
    p name t = do
      unless (HS.member name builtinTypeNames)
        (putStrLn $ name <> " :: " <> show t)

    builtinTypeNames :: HS.HashSet String
    builtinTypeNames = HS.fromList $ HM.keys builtinTypes
