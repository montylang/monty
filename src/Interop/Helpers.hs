module Interop.Helpers where

import RunnerTypes

ordToVal :: Ordering -> Value
ordToVal a = VTypeInstance "Ordering" (show a) []
