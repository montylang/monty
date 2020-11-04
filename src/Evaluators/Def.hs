module Evaluators.Def where

import Text.Megaparsec hiding (Pos)
import Data.List
import Lens.Micro.Platform

import ParserTypes
import RunnerTypes
import Evaluators.Evaluatable
import PrettyPrint
import RunnerUtils
import Debug.Trace

data RDef = RDef
  { rDefPos :: SourcePos,
    rDefArgs :: [Arg],
    rDefBody :: [ET]
  }

instance Evaluatable RDef where
  getPos RDef {rDefPos} = rDefPos
  evaluate RDef {rDefArgs, rDefBody} = do
    types    <- sequence $ argToType <$> rDefArgs
    let fcase = FunctionCase rDefArgs (runBody rDefBody)
    VScoped (VFunction $ FunctionImpl [fcase] types) <$> use scope

instance PrettyPrint RDef where
  prettyPrint RDef {rDefArgs, rDefBody} =
    "def(" <> (intercalate ", " $ prettyPrint <$> rDefArgs) <> ")"

runBody :: [ET] -> Scoper Value
runBody body = do
    vals <- sequence $ evaluate <$> body
    pure $ last vals
