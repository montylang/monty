{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
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
groupByPrecedence :: SourcePos -> [InfixOp] -> [(Maybe InfixOp, ExistsMExpr)] -> ExistsMExpr
groupByPrecedence p [] [] = trace "How did I get here?" undefined
groupByPrecedence p [] [(_, x)] = x
groupByPrecedence p [] ((Just op, x):xs) =
  let fun = Exists $ MExprId p (infixInteropName op) in
  Exists $ MExprCall p fun [x, groupByPrecedence p [] xs]
groupByPrecedence p (o:os) xs = joinHeadOp subCases
  where
    subCases :: [ExistsMExpr]
    subCases = groupByPrecedence p os <$>
      multiSpan ((== Just o) . view _1) xs

    joinHeadOp :: [ExistsMExpr] -> ExistsMExpr
    joinHeadOp []  = undefined
    joinHeadOp [y] = y
    joinHeadOp (y:ys) = foldl folderHeadOp y ys

    folderHeadOp :: ExistsMExpr -> ExistsMExpr -> ExistsMExpr
    folderHeadOp (Exists acc) it = let p = (acc ^. mpos) in
      Exists $ MExprCall p (Exists $ MExprId p $ infixInteropName o) [Exists acc, it]

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

semanticInfixChain :: ExistsMExpr -> [(InfixOp, ExistsMExpr)] -> ExistsMExpr
semanticInfixChain (Exists first) rest =
    groupByPrecedence (first ^. mpos) infixSplitOrder ((Nothing, Exists first):maybeRest)
  where
    maybeTup :: (InfixOp, ExistsMExpr) -> (Maybe InfixOp, ExistsMExpr)
    maybeTup (op, expr) = (Just op, expr) 

    maybeRest :: [(Maybe InfixOp, ExistsMExpr)]
    maybeRest = maybeTup <$> rest

semanticInfix :: PExpr -> InfixOp -> PExpr -> ParseExcept ExistsMExpr
semanticInfix lhs op rhs = do
  semanticLhs <- semantic lhs
  semanticRhs <- infixFlatten op rhs
  pure $ semanticInfixChain semanticLhs semanticRhs

infixFlatten :: InfixOp -> PExpr -> ParseExcept [(InfixOp, ExistsMExpr)]
infixFlatten op (Pos p (ExprInfix lhs nextOp rhs)) = do
  semanticLhs  <- semantic lhs
  semanticRest <- infixFlatten nextOp rhs
  pure $ (op, semanticLhs):semanticRest
infixFlatten op rhs = do
  semanticRhs  <- semantic rhs
  pure [(op, semanticRhs)]

semanticUnwrap :: [PExpr] -> ParseExcept ExistsMExpr
semanticUnwrap [] = throwError $ ErrString "Empty unwrap body"
semanticUnwrap [Pos p (ExprBind _ _)] =
  throwError $ ErrPos p "Cannot have bind on last line of unwrap"
semanticUnwrap [Pos p (ExprAssignment _ _)] =
  throwError $ ErrPos p "Cannot have assignment on last line of unwrap"
semanticUnwrap [last] = semantic last
semanticUnwrap ((Pos p (ExprBind arg expr)):xs) = do
  semanticExpr <- semantic expr
  recursive    <- semanticUnwrap xs

  pure $ Exists $ MExprCall p
    (Exists $ MExprId p "bind")
    [semanticExpr, Exists $ MExprDef p [arg] [recursive]]

semanticUnwrap (expr@(Pos p (ExprAssignment _ _)):xs) = do
  semanticExpr <- semantic expr
  recursive    <- semanticUnwrap xs

  pure $ Exists $ MExprBlock p [semanticExpr, recursive]
semanticUnwrap ((Pos p expr):xs) = do
  semanticUnwrap $ Pos p (ExprBind (IdArg "_") (Pos p expr)):xs

semanticCondBlock :: CondBlock PExpr -> ParseExcept (CondBlock ExistsMExpr)
semanticCondBlock (CondBlock cond body) = do
  newCond <- semantic cond
  newBody <- sequence $ semantic <$> body
  pure $ CondBlock newCond newBody

semanticAss :: (PExpr -> ParseExcept ExistsMExpr) -> PExpr -> ParseExcept ExistsMExpr
semanticAss rhsSemantic (Pos p (ExprAssignment arg value)) = do
  rhs <- rhsSemantic value
  pure $ Exists $ MExprAssignment p arg rhs
semanticAss _ (Pos p _) = throwError $ ErrPos p "Not an assignment"

rearrangeCond :: SourcePos
              -> CondBlock PExpr
              -> [CondBlock PExpr]
              -> [PExpr]
              -> ParseExcept ExistsMExpr
rearrangeCond p ifCond elifConds elseBody = do
  newIfCond    <- rearrangeCondBlock ifCond
  newElifConds <- sequence $ rearrangeCondBlock <$> elifConds
  newElseBody  <- rearrangeReturn elseBody
  pure $ Exists $ MExprIfElse p newIfCond newElifConds newElseBody
  where
    rearrangeCondBlock :: CondBlock PExpr -> ParseExcept (CondBlock ExistsMExpr)
    rearrangeCondBlock (CondBlock cond body) =
      liftM2 CondBlock (semantic cond) (rearrangeReturn body)

-- Rearranges a function such that the last statement will be a return
rearrangeReturn :: [PExpr] -> ParseExcept [ExistsMExpr]
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

semanticDef :: Maybe Id -> PExpr -> ParseExcept ExistsMExpr
semanticDef name (Pos p (ExprDef args body)) = do
  newBody <- rearrangeReturn body
  pure $ Exists $ MExprDef p args newBody
semanticDef _ (Pos p _) = throwError $ ErrPos p "Not a def"

semantic :: PExpr -> ParseExcept ExistsMExpr
-- Actual semantic alterations
semantic (Pos p (ExprUnwrap body)) = do
  --semanticBodies <- sequence $ semantic <$> body
  semanticUnwrap body

semantic (Pos p (ExprInfix lhs op rhs)) =
  semanticInfix lhs op rhs

-- Traversals
semantic (Pos p (ExprPrefixOp op ex)) = do
  semanticEx <- semantic ex
  pure $ Exists $ MExprCall p (Exists $ MExprId p $ interopName op) [semanticEx]
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
  pure $ Exists $ MExprIfElse p newIfCond newElifConds newElseBody
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
  pure $ Exists $ MExprCall p newFunc newParams
semantic (Pos p (ExprId name)) = do
  pure $ Exists $ MExprId p name
semantic (Pos p (ExprChar value)) = do
  pure $ Exists $ MExprChar p value
semantic (Pos p (ExprInt value)) = do
  pure $ Exists $ MExprInt p value
semantic (Pos p (ExprDouble value)) = do
  pure $ Exists $ MExprDouble p value

semantic (Pos p other) = throwError $ ErrPos p $
  "Unexpected expr in semnatic: " <> show other
