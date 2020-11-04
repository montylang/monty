{-# LANGUAGE ExistentialQuantification #-}
module Evaluators.Evaluatable where

import Text.Megaparsec (SourcePos)

import RunnerTypes

class Evaluatable a where
  evaluate :: a -> Scoper Value
  getPos :: a -> SourcePos

data ET = forall a. Evaluatable a => ET a

instance Evaluatable ET where
  evaluate (ET thing) = evaluate thing
  getPos (ET thing)   = getPos thing
