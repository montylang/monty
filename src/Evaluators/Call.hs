module Evaluators.Call where

import Text.Megaparsec hiding (Pos)
import Data.List
import Control.Lens

import ParserTypes
import RunnerTypes
import Evaluators.Evaluatable
import PrettyPrint
import RunnerUtils
import CallableUtils

data RCall = RCall
  { rCallPos :: SourcePos,
    rCallFun :: ET,
    rCallParams :: [ET]
  }

instance Evaluatable RCall where
  getPos RCall {rCallPos} = rCallPos
  evaluate rcall@RCall {rCallFun, rCallParams} = do
      pushToCallStack
      fun        <- eval rCallFun
      evaledArgs <- sequence $ eval <$> rCallParams
      runFun fun evaledArgs <* popFromCallStack
    where
      pushToCallStack :: Scoper ()
      pushToCallStack = callStack %= (rCallPos rcall:)
  
      popFromCallStack :: Scoper ()
      popFromCallStack = callStack %= drop 1

instance PrettyPrint RCall where
  prettyPrint _ = "<function call>"
