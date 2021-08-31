module Interop.Helpers where

import RunnerTypes

ordToVal :: Ordering -> Value
ordToVal a = VTypeInstance "Ordering" (show a) []

stringToVal :: String -> Value
stringToVal s = VList $ VChar <$> s
