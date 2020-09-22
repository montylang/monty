module RunnerTypes where

import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import ParserTypes
import Text.Megaparsec.Pos (SourcePos)

type ScopeBlock = HM.HashMap Id Value
type Scope      = [ScopeBlock]

type Scoper a = StateT Scope IO a

type EValue = Either String Value

-- listMapArgHead = "head"
-- listMapArgTail = "tail"

-- listMapBody :: Scoper Value
-- listMapBody = do
--   head <- findInScope listMapArgHead
--   tail <- findInScope "tail"

-- a = InteropCase [PatternArg "Just" [IdArg "wubalubadubdub"]] isJust

data FunctionCase
  = FunctionCase { fcaseArgs :: [Arg], fcaseBody :: [PExpr] }
  | InteropCase { fcaseArgs :: [Arg], fcaseInteropBody :: (Scoper Value) }

instance Show FunctionCase where
  show fcase = "def (" <> intercalate "," (show <$> args) <> ")"
    where args = fcaseArgs fcase

instance Eq FunctionCase where
  (==) first second = fcaseArgs first == fcaseArgs second

data Value
  = VInt Int
  | VString String
  | VBoolean Bool
  | VFunction [FunctionCase]
  | VTypeCons {
      getVTypeConsClass :: Id,
      getVTypeConsName :: Id,
      getVTypeConsArgs :: [Id]
    }
  | VTypeInstance {
      getTypeInstanceClass :: Id,
      getTypeInstanceName :: Id,
      getTypeInstanceVals :: [Value]
    }
  -- Name, [Arg]
  -- Arg must contain _one_ instance of the word "self", for pattern matching
  | VTypeDef Id [DefSignature]
  | VTypeFunction {
      fTypeName :: Id,
      fFuncName :: Id,
      fArgs :: [Arg],
      fFuncCases :: [FunctionCase]
    }
  | VScoped Value Scope
  | VClass [Id]
  | VList [Value]
  | VDict    
  | VTuple   
  | VError [SourcePos] String -- aka die aka move on to a better place (hopefully)
  deriving (Eq)

instance Show Value where
  show (VInt value) = show value
  show (VString value) = show value
  show (VBoolean value) = show value
  show (VFunction _) = show "<function>"
  show (VTypeCons _ name args) =
    name <>
    if length args == 0
      then ""
      else "(" <> intercalate "," args <> ")"
  show (VTypeInstance _ name vals) =
    name <>
    if length vals == 0
      then ""
      else "(" <> intercalate "," (show <$> vals) <> ")"
  show (VTypeDef name _) = "<type " <> name <> ">"
  show (VTypeFunction _ _ _ _) = "<function>"
  show (VScoped value _) = show value
  show (VClass _) = "<class>"
  show (VList values) = show values
  show (VDict) = undefined
  show (VTuple) = undefined
  show (VError _ message) = "<error " <> message <> ">"
