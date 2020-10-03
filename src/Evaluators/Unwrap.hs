module Evaluators.Unwrap (evalUnwrap) where

import Data.Maybe

import RunnerTypes
import ParserTypes
import RunnerUtils
import CallableUtils

evalUnwrap :: [PExpr] -> Scoper Value
evalUnwrap = (evalP =<<) . evalUnwrap'

evalUnwrap' :: [PExpr] -> Scoper PExpr
evalUnwrap' []     = stackTrace "Empty unwrap body"
evalUnwrap' [(Pos _ (ExprBind _ _))] =
  stackTrace "Cannot have bind on last line of unwrap"
evalUnwrap' [last] = pure last
evalUnwrap' ((Pos a (ExprBind arg expr)):xs) = do
  recursive <- evalUnwrap' xs
  pure $ Pos a $ ExprCall
    (Pos a (ExprId "bind"))
    [expr, Pos a $ ExprDef [IdArg arg] [addReturn recursive]]
evalUnwrap' _ = stackTrace "I just shit myself"

addReturn :: PExpr -> PExpr
addReturn (Pos a e) = Pos a $ ExprReturn (Pos a e)
