module MorphUtils where

import Control.Lens
import Debug.Trace

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
