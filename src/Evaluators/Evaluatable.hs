{-# LANGUAGE ExistentialQuantification #-}
module Evaluators.Evaluatable where

import Text.Megaparsec (SourcePos)

import Control.Lens
import RunnerTypes

class Evaluatable a where
  render :: a -> String

data ET = forall a. Evaluatable a => ET a

instance Evaluatable ET where
  render (ET thing)   = render thing
