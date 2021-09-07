{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Parser.Semantic (ParseErr(..), ParseExcept, semantic) where

import Data.Maybe
import Data.Void
import Debug.Trace
import System.Exit
import Text.Megaparsec hiding (Pos)
import Control.Monad.Except
import Control.Lens

import ParserTypes hiding (RExpr)
import RunnerUtils
import CallableUtils
import MorphUtils

import TypeUtils (pexprToTree)
import MiddleEndTypes

data ParseErr
  = ErrPos SourcePos String
  | ErrString String
  | ErrParse (ParseErrorBundle String Void)

instance Show ParseErr where
  show (ErrParse bundle) = errorBundlePretty bundle
  show (ErrPos p message) = message <> " at " <> show p
  show (ErrString message) = message

type ParseExcept = Except ParseErr

-- These are listed in inverse precedence order
infixSplitOrder :: [InfixOp]
infixSplitOrder = [
    InfixMappend,
    InfixLogicOr,
    InfixLogicAnd,
    InfixLt,
    InfixGt,
    InfixGe,
    InfixLe,
    InfixEq,
    InfixEq,
    InfixNe,
    InfixSub,
    InfixAdd,
    InfixMod,
    InfixDiv,
    InfixMul
  ]

-- FIXME: better solution pls
wasteSourcePos :: SourcePos
wasteSourcePos = SourcePos "" (mkPos maxBound) (mkPos maxBound)

-- What a mess
groupByPrecedence :: SourcePos -> [InfixOp] -> [(Maybe InfixOp, MExpr)] -> MExpr
groupByPrecedence p [] [] = trace "How did I get here?" undefined
groupByPrecedence p [] [(_, x)] = x
groupByPrecedence p [] ((Just op, x):xs) =
  let fun = MExprId p (infixInteropName op) in
  MExprCall p MUnknown fun [x, groupByPrecedence p [] xs]
groupByPrecedence p (o:os) xs = joinHeadOp subCases
  where
    subCases :: [MExpr]
    subCases = groupByPrecedence p os <$>
      multiSpan ((== Just o) . view _1) xs

    joinHeadOp :: [MExpr] -> MExpr
    joinHeadOp []  = undefined
    joinHeadOp [y] = y
    joinHeadOp (y:ys) = foldl folderHeadOp y ys

    folderHeadOp :: MExpr -> MExpr -> MExpr
    folderHeadOp acc it = let p = (acc ^. mpos) in
      MExprCall p MUnknown (MExprId p $ infixInteropName o) [acc, it]

infixInteropName :: InfixOp -> Id
infixInteropName InfixAdd      = "#add"
infixInteropName InfixSub      = "#subtract"
infixInteropName InfixMul      = "#multiply"
infixInteropName InfixDiv      = "#divide"
infixInteropName InfixMod      = "#modulus"
infixInteropName InfixEq       = "#equals"
infixInteropName InfixNe       = "#notequals"
infixInteropName InfixGt       = "#greater"
infixInteropName InfixLt       = "#less"
infixInteropName InfixLe       = "#lessequal"
infixInteropName InfixGe       = "#greaterequal"
infixInteropName InfixLogicAnd = "#and"
infixInteropName InfixLogicOr  = "#or"
-- TODO: just cal the functions for these
infixInteropName InfixCons     = "#cons"
infixInteropName InfixMappend  = "#mappend"

semanticInfixChain :: MExpr -> [(InfixOp, MExpr)] -> MExpr
semanticInfixChain first rest =
    groupByPrecedence (first ^. mpos) infixSplitOrder ((Nothing, first):maybeRest)
  where
    maybeTup :: (InfixOp, MExpr) -> (Maybe InfixOp, MExpr)
    maybeTup (op, expr) = (Just op, expr) 

    maybeRest :: [(Maybe InfixOp, MExpr)]
    maybeRest = maybeTup <$> rest

semanticInfix :: PExpr -> InfixOp -> PExpr -> ParseExcept MExpr
semanticInfix lhs op rhs = do
  semanticLhs <- semantic lhs
  semanticRhs <- infixFlatten op rhs
  pure $ semanticInfixChain semanticLhs semanticRhs

infixFlatten :: InfixOp -> PExpr -> ParseExcept [(InfixOp, MExpr)]
infixFlatten op (Pos p (ExprInfix lhs nextOp rhs)) = do
  semanticLhs  <- semantic lhs
  semanticRest <- infixFlatten nextOp rhs
  pure $ (op, semanticLhs):semanticRest
infixFlatten op rhs = do
  semanticRhs  <- semantic rhs
  pure [(op, semanticRhs)]

semanticUnwrap :: [PExpr] -> ParseExcept MExpr
semanticUnwrap [] = throwError $ ErrString "Empty unwrap body"
semanticUnwrap [Pos p (ExprBind _ _)] =
  throwError $ ErrPos p "Cannot have bind on last line of unwrap"
semanticUnwrap [Pos p (ExprAssignment _ _)] =
  throwError $ ErrPos p "Cannot have assignment on last line of unwrap"
semanticUnwrap [last] = semantic last
semanticUnwrap ((Pos p (ExprBind arg expr)):xs) = do
  semanticExpr <- semantic expr
  recursive    <- semanticUnwrap xs

  pure $ MExprCall p
    MUnknown
    (MExprId p "bind")
    [semanticExpr, MExprDef p Nothing [arg] [recursive]]

semanticUnwrap (expr@(Pos p (ExprAssignment _ _)):xs) = do
  semanticExpr <- semantic expr
  recursive    <- semanticUnwrap xs

  pure $ MExprBlock p MUnknown [semanticExpr, recursive]
semanticUnwrap ((Pos p expr):xs) = do
  semanticUnwrap $ Pos p (ExprBind (IdArg "_") (Pos p expr)):xs

semanticCondBlock :: CondBlock PExpr -> ParseExcept (CondBlock MExpr)
semanticCondBlock (CondBlock cond body) = do
  newCond <- semantic cond
  newBody <- sequence $ semantic <$> body
  pure $ CondBlock newCond newBody

semanticAss :: (PExpr -> ParseExcept MExpr) -> PExpr -> ParseExcept MExpr
semanticAss rhsSemantic (Pos p (ExprAssignment arg value)) = do
  rhs <- rhsSemantic value
  pure $ MExprAssignment p MUnknown arg rhs
semanticAss _ (Pos p _) = throwError $ ErrPos p "Not an assignment"

rearrangeCond :: SourcePos
              -> CondBlock PExpr
              -> [CondBlock PExpr]
              -> [PExpr]
              -> ParseExcept MExpr
rearrangeCond p ifCond elifConds elseBody = do
  newIfCond    <- rearrangeCondBlock ifCond
  newElifConds <- sequence $ rearrangeCondBlock <$> elifConds
  newElseBody  <- rearrangeReturn elseBody
  pure $ MExprIfElse p MUnknown newIfCond newElifConds newElseBody
  where
    rearrangeCondBlock :: CondBlock PExpr -> ParseExcept (CondBlock MExpr)
    rearrangeCondBlock (CondBlock cond body) =
      liftM2 CondBlock (semantic cond) (rearrangeReturn body)

-- Rearranges a function such that the last statement will be a return
rearrangeReturn :: [PExpr] -> ParseExcept [MExpr]
rearrangeReturn [] = throwError $ ErrString "Did not find expected return statement"
rearrangeReturn [Pos p (ExprReturn retExpr)] = pure <$> semantic retExpr 
rearrangeReturn ((Pos p (ExprIfElse fi file esle)):xs) | containsReturn fi =
  pure <$> case esle of
    Just elseBody -> rearrangeCond p fi file elseBody
    _             -> rearrangeCond p fi file xs
  where
    containsReturn :: CondBlock PExpr -> Bool
    containsReturn (CondBlock _ body) =
      or (treeHasReturn . pexprToTree <$> body)

    treeHasReturn :: Tree PExpr -> Bool
    treeHasReturn (Tree (Pos _ (ExprDef _ _)) _)  = False
    treeHasReturn (Tree (Pos _ (ExprReturn _)) _) = True
    treeHasReturn (Tree _ subtrees)               = or (treeHasReturn <$> subtrees)

rearrangeReturn (x:xs) =
  liftM2 (:) (semantic x) (rearrangeReturn xs)

semanticDef :: Maybe Id -> PExpr -> ParseExcept MExpr
semanticDef name (Pos p (ExprDef args body)) = do
  newBody <- rearrangeReturn body
  pure $ MExprDef p Nothing args newBody
semanticDef _ (Pos p _) = throwError $ ErrPos p "Not a def"

semantic :: PExpr -> ParseExcept MExpr
-- Actual semantic alterations
semantic (Pos p (ExprUnwrap body)) = do
  --semanticBodies <- sequence $ semantic <$> body
  semanticUnwrap body

semantic (Pos p (ExprInfix lhs op rhs)) =
  semanticInfix lhs op rhs

-- Traversals
semantic (Pos p (ExprPrefixOp op ex)) = do
  semanticEx <- semantic ex
  pure $ MExprCall p MUnknown (MExprId p $ interopName op) [semanticEx]
  where
    interopName :: PrefixOp -> Id
    interopName PrefixNot = "#not"
    interopName PrefixNegate = "#negate"
semantic (Pos _ (ExprPrecedence inner)) =
  semantic inner
semantic (Pos p (ExprIfElse ifCond elifConds elseBody)) = do
  elseBody'    <- getOrDie elseBody
  newIfCond    <- semanticCondBlock ifCond
  newElifConds <- sequence $ semanticCondBlock <$> elifConds
  newElseBody  <- sequence $ semantic          <$> elseBody'
  pure $ MExprIfElse p MUnknown newIfCond newElifConds newElseBody
  where
    getOrDie :: Maybe [PExpr] -> ParseExcept [PExpr]
    getOrDie (Just xs) = pure xs
    getOrDie _ =
      throwError $ ErrPos p "Non-returning conditional must have an else clause"
semantic ass@(Pos _ (ExprAssignment arg@(IdArg name) value@(Pos _ (ExprDef _ _)))) = do
  semanticAss (semanticDef $ Just name) ass
semantic ass@(Pos _ (ExprAssignment _ _)) = do
  semanticAss semantic ass
semantic def@(Pos _ ExprDef {}) = do
  semanticDef Nothing def
semantic (Pos p (ExprCall func params)) = do
  newFunc <- semantic func 
  newParams <- sequence $ semantic <$> params 
  pure $ MExprCall p MUnknown newFunc newParams
semantic (Pos p (ExprId name)) = do
  pure $ MExprId p name
semantic (Pos p (ExprChar value)) = do
  pure $ MExprChar p value
semantic (Pos p (ExprInt value)) = do
  pure $ MExprInt p value
semantic (Pos p (ExprDouble value)) = do
  pure $ MExprDouble p value

semantic (Pos p other) = throwError $ ErrPos p $
  "Unexpected expr in semnatic: " <> show other
