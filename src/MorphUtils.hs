module MorphUtils where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ []  = []
splitWhen _ [x] = [[x]]
splitWhen f xs  =
  let (ys, rest) = span (not . f) xs
  in case ys of
    [] -> [rest]
    zs -> [zs] <> (splitWhen f rest)
