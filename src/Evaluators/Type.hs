module Evaluators.Type where

import Evaluators.Evaluatable
import Text.Megaparsec hiding (Pos)
import qualified Data.HashMap.Strict as HM

import PrettyPrint
import RunnerTypes
import ParserTypes
import RunnerUtils

data RType = RType
  { rTypePos :: SourcePos,
    rTypeName :: Id,
    rTypeSigs :: [Pos DefSignature]
  }

instance Evaluatable RType where
  render _ = "<type>"

evalType :: Id -> [Pos DefSignature] -> Scoper Value
evalType typeName headers = do
    addToTypeScope typeName typeDef
    unionTopScope $ defSigToKeyValue . getPosValue <$> headers
    pure unitValue
  where
    typeDef = VTypeDef typeName $ getPosValue <$> headers

    defSigToKeyValue :: DefSignature -> (Id, Value)
    defSigToKeyValue defSig =
      (getDefSigFunName defSig, VTypeFunction defSig HM.empty)

instance PrettyPrint RType where
  prettyPrint RType {rTypeName} = "<type " <> rTypeName <> ">"
