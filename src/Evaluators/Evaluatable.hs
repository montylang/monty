{-# LANGUAGE ExistentialQuantification #-}
module Evaluators.Evaluatable where

import Text.Megaparsec (SourcePos)

import Control.Lens
import RunnerTypes

class Evaluatable a where
  evaluate :: a -> Scoper Value
  getPos :: a -> SourcePos

data ET = forall a. Evaluatable a => ET a

instance Evaluatable ET where
  evaluate (ET thing) = evaluate thing
  getPos (ET thing)   = getPos thing

eval :: Evaluatable a => a -> Scoper Value
eval thing = do
  currentPos .= getPos thing
  evaluate thing
