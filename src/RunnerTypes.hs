module RunnerTypes where

import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import ParserTypes

type ScopeBlock = HM.HashMap Id Value
type Scope      = [ScopeBlock]

type Scoper a = StateT Scope IO a

data FunctionCase
  = FunctionCase [Arg] [Expr] 
  -- TODO: 
  -- | InteropCase ([Value] -> Scoper Value)
  deriving (Show, Eq)

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
      getTypeName :: Id,
      getFuncName :: Id,
      getArgs :: [Id],
      getFuncCases :: [FunctionCase]
    }
  | VScoped Value Scope
  | VClass
  | VList [Value]
  | VDict    
  | VTuple   
  deriving (Show, Eq)
