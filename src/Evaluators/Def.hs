module Evaluators.Def where

import Text.Megaparsec hiding (Pos)
import Data.List
import Control.Lens

import ParserTypes
import RunnerTypes
import Evaluators.Evaluatable
import PrettyPrint
import RunnerUtils
import Debug.Trace

data RDef = RDef
  { _rDefPos :: SourcePos,
    _rDefArgs :: [Arg],
    _rDefBody :: [ET]
  }

(makeLenses ''RDef)

instance Evaluatable RDef where
  getPos def = def ^. rDefPos
  evaluate (RDef _ args body) = do
    types    <- sequence $ argToType <$> args
    let fcase = FunctionCase args (runBody body)
    VScoped (VFunction $ FunctionImpl [fcase] types) <$> use scope

instance PrettyPrint RDef where
  prettyPrint (RDef _ args body) =
    "def(" <> (intercalate ", " $ prettyPrint <$> args) <> ")"

runBody :: [ET] -> Scoper Value
runBody [b]    = evaluate b
runBody (b:bs) = evaluate b *> runBody bs
