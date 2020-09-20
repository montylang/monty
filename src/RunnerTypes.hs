module RunnerTypes where

import Data.List (intercalate)
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import ParserTypes
import Text.Parsec.Pos

type ScopeBlock = HM.HashMap Id Value
type Scope      = [ScopeBlock]

type Scoper a = StateT Scope IO a

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
  | VTypeCons Id [Id]
  | VTypeInstance Id [Value] -- Id = Type name
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
  deriving (Show, Eq)
