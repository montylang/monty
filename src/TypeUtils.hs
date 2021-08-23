module TypeUtils where

import Debug.Trace

import RunnerUtils
import RunnerTypes
import ParserTypes
import Control.Monad
import Data.Maybe

vtrue :: Value
vtrue  = VTypeInstance "Bool" "True" []

vfalse :: Value
vfalse = VTypeInstance "Bool" "False" []

toBoolValue :: Bool -> Value
toBoolValue True  = vtrue
toBoolValue False = vfalse

valueToBool :: Value -> Scoper Bool
valueToBool (VTypeInstance "Bool" "True" _)  = pure True
valueToBool (VTypeInstance "Bool" "False" _) = pure False
valueToBool v = stackTrace $ show v <> " is not a bool"

isVInstanceNamed :: Id -> Value -> Bool
isVInstanceNamed expected (VTypeInstance _ iname _) = iname == expected
isVInstanceNamed _ _ = False

isVInstanceOf :: Id -> Value -> Bool
isVInstanceOf expected (VTypeInstance tname _ _) = tname == expected
isVInstanceOf _ _ = False

pexprToTree :: PExpr -> Tree PExpr
pexprToTree e@(Pos _ (ExprIfElse ifBlock elifs elseBody)) = Tree e subtrees
  where
    subtrees =
      condBlockToTrees ifBlock <>
      (condBlockToTrees =<< elifs) <>
      (pexprToTree <$> join (maybeToList elseBody))
    condBlockToTrees :: CondBlock PExpr -> [Tree PExpr]
    condBlockToTrees (CondBlock condExpr condBody) =
      pexprToTree condExpr : (pexprToTree <$> condBody)
pexprToTree e@(Pos _ (ExprCase caseExpr caseBlocks)) = Tree e subtrees
  where
    subtrees  = exprTree : bodyTrees
    exprTree  = pexprToTree caseExpr
    bodyTrees = pexprToTree <$> (cbbody =<< caseBlocks)
pexprToTree e@(Pos _ (ExprInfix lhs _ rhs))      = Tree e (pexprToTree <$> [lhs, rhs])
pexprToTree e@(Pos _ (ExprPrefixOp _ expr))      = Tree e [pexprToTree expr]
pexprToTree e@(Pos _ (ExprAssignment _ expr))    = Tree e [pexprToTree expr]
pexprToTree e@(Pos _ (ExprDef _ exprs))          = Tree e (pexprToTree <$> exprs)
pexprToTree e@(Pos _ (ExprCall exprF params))    = Tree e (pexprToTree exprF : (pexprToTree <$> params))
pexprToTree e@(Pos _ (ExprReturn expr))          = Tree e [pexprToTree expr]
pexprToTree e@(Pos _ (ExprList exprs))           = Tree e (pexprToTree <$> exprs)
pexprToTree e@(Pos _ (ExprTuple exprs))          = Tree e (pexprToTree <$> exprs)
pexprToTree e@(Pos _ (ExprInstanceOf _ _ exprs)) = Tree e (pexprToTree <$> exprs)
pexprToTree e@(Pos _ (ExprBind _ expr))          = Tree e [pexprToTree expr]
pexprToTree e@(Pos _ (ExprUnwrap exprs))         = Tree e (pexprToTree <$> exprs)
pexprToTree e@(Pos _ (ExprPrecedence expr))      = Tree e [pexprToTree expr]
pexprToTree e                                    = Tree e []
