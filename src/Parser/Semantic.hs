module Parser.Semantic (ParseErr(..), ParseExcept, semantic) where

import Data.Maybe
import Data.Void
import Debug.Trace
import System.Exit
import Text.Megaparsec hiding (Pos)
import Control.Monad.Except
import Lens.Micro.Platform

import ParserTypes
import RunnerUtils
import CallableUtils
import MorphUtils

data ParseErr
  = ErrPos SourcePos String
  | ErrString String
  | ErrParse (ParseErrorBundle String Void)

instance Show ParseErr where
  show (ErrParse bundle) = errorBundlePretty bundle
  show (ErrPos p message) = message <> " at " <> show p
  show (ErrString message) = message

type ParseExcept = Except ParseErr

infixPrecedence :: [InfixOp]
infixPrecedence = [
    InfixMappend,
    InfixAdd,
    InfixSub,
    InfixMul,
    InfixDiv,
    InfixMod,
    InfixNe,
    InfixEq,
    InfixEq,
    InfixLe,
    InfixGe,
    InfixGt,
    InfixLt,
    InfixLogicAnd,
    InfixLogicOr
  ]

-- What a mess
groupByPrecedence :: [InfixOp] -> [(Maybe InfixOp, PExpr)] -> PExpr
groupByPrecedence [] [] = trace "How did I get here?" undefined
groupByPrecedence [] [(_, x)] = x
groupByPrecedence [] ((Just op, (Pos p x)):xs) =
  -- TODO: Nothing about this is ok
  Pos p $ ExprInfix (Pos p x) op $ groupByPrecedence [] xs
groupByPrecedence (o:os) xs  = joinHeadOp subCases
  where
    subCases :: [PExpr]
    subCases = groupByPrecedence os <$>
      (multiSpan ((== (Just o)) . (view _1)) xs)

    joinHeadOp :: [PExpr] -> PExpr
    joinHeadOp [y] = y
    joinHeadOp (y:ys) = foldl folderHeadOp y ys

    folderHeadOp :: PExpr -> PExpr -> PExpr
    folderHeadOp acc it = Pos (getPos it) $ ExprInfix acc o it

semanticInfixChain :: PExpr -> [(InfixOp, PExpr)] -> PExpr
semanticInfixChain first rest =
    groupByPrecedence infixPrecedence ((Nothing, first):maybeRest)
  where
    maybeTup :: (InfixOp, PExpr) -> (Maybe InfixOp, PExpr)
    maybeTup (op, expr) = (Just op, expr) 

    maybeRest :: [(Maybe InfixOp, PExpr)]
    maybeRest = maybeTup <$> rest

semanticInfix :: PExpr -> InfixOp -> PExpr -> ParseExcept PExpr
semanticInfix lhs op rhs = do
  semanticLhs <- semantic lhs
  semanticRhs <- infixFlatten op rhs
  pure $ semanticInfixChain semanticLhs semanticRhs

infixFlatten :: InfixOp -> PExpr -> ParseExcept [(InfixOp, PExpr)]
infixFlatten op (Pos p (ExprInfix lhs nextOp rhs)) = do
  semanticLhs  <- semantic lhs
  semanticRest <- infixFlatten nextOp rhs
  pure $ (op, semanticLhs):semanticRest
infixFlatten op rhs = do
  semanticRhs  <- semantic rhs
  pure [(op, semanticRhs)]

semanticUnwrap :: [PExpr] -> ParseExcept PExpr
semanticUnwrap [] = throwError $ ErrString "Empty unwrap body"
semanticUnwrap [(Pos p (ExprBind _ _))] =
  throwError $ ErrPos p "Cannot have bind on last line of unwrap"
semanticUnwrap [last] = pure last
semanticUnwrap ((Pos p (ExprBind arg expr)):xs) = do
  recursive <- semanticUnwrap xs
  pure $ Pos p $ ExprCall
    (Pos p (ExprId "bind"))
    [expr, Pos p $ ExprDef [arg] [addReturn recursive]]
semanticUnwrap ((Pos p _):_) =
  throwError $ ErrPos p "All non-tails in unwrap must be binds"

addReturn :: PExpr -> PExpr
addReturn (Pos a e) = Pos a $ ExprReturn (Pos a e)

semanticCondBlock :: CondBlock -> ParseExcept CondBlock
semanticCondBlock (CondBlock cond body) = do
  newCond <- semantic cond
  newBody <- sequence $ semantic <$> body
  pure $ CondBlock newCond newBody

semanticCaseBlock :: Pos CaseBlock -> ParseExcept (Pos CaseBlock)
semanticCaseBlock (Pos p (CaseBlock arg body)) = do
  newBody <- sequence $ semantic <$> body
  pure $ Pos p $ CaseBlock arg newBody

semantic :: PExpr -> ParseExcept PExpr
-- Actual semantic alterations
semantic (Pos p (ExprUnwrap body)) = do
  semanticBodies <- sequence $ semantic <$> body
  semanticUnwrap semanticBodies

semantic (Pos p (ExprInfix lhs op rhs)) =
  semanticInfix lhs op rhs

-- Traversals
semantic (Pos _ (ExprPrecedence inner)) =
  semantic inner
semantic (Pos p (ExprIfElse ifCond elifConds elseBody)) = do
  newIfCond    <- semanticCondBlock ifCond 
  newElifConds <- sequence $ semanticCondBlock <$> elifConds 
  newElseBody  <- sequence $ semantic          <$> elseBody
  pure $ Pos p $ ExprIfElse newIfCond newElifConds newElseBody
semantic (Pos p (ExprAssignment arg value)) = do
  newValue <- semantic value
  pure $ Pos p $ ExprAssignment arg newValue
semantic (Pos p (ExprDef args body)) = do
  newBody <- sequence $ semantic <$> body
  pure $ Pos p $ ExprDef args newBody
semantic (Pos p (ExprCall func params)) = do
  newFunc <- semantic func 
  newParams <- sequence $ semantic <$> params 
  pure $ Pos p $ ExprCall newFunc newParams
semantic (Pos p (ExprReturn retVal)) = do
  newRetVal <- semantic retVal 
  pure $ Pos p $ ExprReturn newRetVal
semantic (Pos p (ExprList elements)) = do
  newElements <- sequence $ semantic <$> elements 
  pure $ Pos p $ ExprList newElements
semantic (Pos p (ExprTuple elements)) = do
  newElements <- sequence $ semantic <$> elements 
  pure $ Pos p $ ExprTuple newElements
semantic (Pos p (ExprInstanceOf tname cname elements)) = do
  newElements <- sequence $ semantic <$> elements 
  pure $ Pos p $ ExprInstanceOf tname cname newElements
semantic (Pos p (ExprBind arg value)) = do
  newValue <- semantic value 
  pure $ Pos p $ ExprBind arg newValue
semantic (Pos p (ExprCase input blocks)) = do
  newInput <- semantic input
  newBlocks <- sequence $ semanticCaseBlock <$> blocks
  pure $ Pos p $ ExprCase newInput newBlocks
semantic other = pure other
