module MorphUtils where

import Lens.Micro.Platform
import Debug.Trace

multiSpan :: (a -> Bool) -> [a] -> [[a]]
multiSpan _ []  = []
multiSpan _ [x] = [[x]]
multiSpan f (x:xs) =
    let (ys, rest) = (multiSpan' . span (not . f)) remainder
      in [header <> ys] <> rest
  where
    multiSpan' (xs, ys) = (xs, multiSpan f ys)

    cond = f x
    remainder = if cond then xs else (x:xs)
    header = if cond then [x] else []
