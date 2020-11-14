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
    _rDefName :: Maybe Id,
    _rDefArgs :: [Arg],
    _rDefBody :: [ET]
  }

(makeLenses ''RDef)

instance Evaluatable RDef where
  getPos def = def ^. rDefPos
  evaluate (RDef _ name args body) = do
    types    <- sequence $ argToType <$> args
    let fcase = FunctionCase args (runBody body)
    VScoped (VFunction $ FunctionImpl name [fcase] types) <$> use scope

instance PrettyPrint RDef where
  prettyPrint (RDef _ name args body) =
    "def " <> show name <> "(" <> (intercalate ", " $ prettyPrint <$> args) <> ")"

runBody :: [ET] -> Scoper Value
runBody [b]    = eval b
runBody (b:bs) = eval b *> runBody bs
