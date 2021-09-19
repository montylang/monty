module InferenceTypes where

import MyPrelude

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Control.Monad.State
import Control.Monad.Except
import MiddleEndTypes

data TIState = TIState
  { _tyVarCount :: Int
  -- NOTE: these should only be modified at the root level scope
  , _globalDefs :: HM.HashMap String MType
  , _topLevelMExprs :: HM.HashMap String ExistsMExpr }

emptyTIState :: TIState
emptyTIState = TIState 0 HM.empty HM.empty

type TI a = ExceptT String (State TIState) a

class Types a where
  -- Find all FreeTypeVariables in something
  ftv :: a -> HS.HashSet String
  -- Applies a substitution (of types) to something
  substitute :: Subst -> a -> a

instance Types a => Types [a] where
    substitute s = map (substitute s)
    ftv l        = foldr (HS.union . ftv) HS.empty l

newtype TypeEnv = TypeEnv (HM.HashMap String Scheme)
  deriving (Show)
emptyTypeEnv = TypeEnv HM.empty

instance Types TypeEnv where
  ftv (TypeEnv env)          = ftv (HM.elems env)
  substitute s (TypeEnv env) = TypeEnv (HM.map (substitute s) env)

type Subst = HM.HashMap String MType

nullSubst :: Subst
nullSubst = HM.empty

runTI :: TI a -> (Either String a, TIState)
runTI t = runState (runExceptT t) emptyTIState

data Scheme = Scheme
  { schemaTypeVars :: [String]
  , schemaType :: MType }
  deriving (Show)

instance Types Scheme where
    ftv (Scheme vars t) = HS.difference (ftv t) (HS.fromList vars)
    substitute s (Scheme vars t) = Scheme vars (substitute (foldr HM.delete s vars) t)

instance Types MType where
    ftv MInt        = HS.empty
    ftv MBool       = HS.empty
    ftv (MVar name) = HS.singleton name
    ftv (MFun inputType outputType) =
      HS.unions $ ftv <$> [outputType, inputType]
    ftv (MFunZero outputType) = ftv outputType

    substitute s (MVar name) = case HM.lookup name s of
                         Nothing  -> MVar name
                         Just t   -> t
    substitute s (MFun t1 t2) = MFun (substitute s t1) (substitute s t2)
    substitute s (MFunZero t) = MFunZero (substitute s t)
    substitute s t            = t

$(makeLenses ''TIState)
