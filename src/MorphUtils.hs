{-# LANGUAGE RankNTypes, StandaloneKindSignatures, PolyKinds #-}
module MorphUtils where

import Control.Lens
import Debug.Trace
import Data.Kind (Type)

-- Generic existential wrapper for GADTs
type Exists :: forall k. (k -> Type) -> Type
data Exists f where
  Exists :: f x -> Exists f

multiSpan :: (a -> Bool) -> [a] -> [[a]]
multiSpan _ []  = []
multiSpan _ [x] = [[x]]
multiSpan f (x:xs) =
    let (ys, rest) = (multiSpan' . break f) remainder
      in [header <> ys] <> rest
  where
    multiSpan' (xs, ys) = (xs, multiSpan f ys)

    cond = f x
    remainder = if cond then xs else x:xs
    header = [x | cond]

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a, b, c) = f a b c

-- Replaces a sublist with another sublist
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ l                                       = l
replace source target []                             = []
replace source target xs | length xs < length source = xs
replace source target l@(x:xs) =
  if take (length source) l == source then
    target <> replace source target (drop (length source) l)
  else
    x : replace source target xs
