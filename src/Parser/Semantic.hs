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
groupByPrecedence :: [InfixOp] -> [(Maybe InfixOp, RExpr)] -> RExpr
groupByPrecedence [] [] = trace "How did I get here?" undefined
groupByPrecedence [] [(_, x)] = x
groupByPrecedence [] ((Just op, x):xs) =
  RExprInfix (rpos x) x op $ groupByPrecedence [] xs
groupByPrecedence (o:os) xs = joinHeadOp subCases
  where
    subCases :: [RExpr]
    subCases = groupByPrecedence os <$>
      (multiSpan ((== (Just o)) . (view _1)) xs)

    joinHeadOp :: [RExpr] -> RExpr
    joinHeadOp [y] = y
    joinHeadOp (y:ys) = foldl folderHeadOp y ys

    folderHeadOp :: RExpr -> RExpr -> RExpr
    folderHeadOp acc it = RExprInfix (rpos it) acc o it

semanticInfixChain :: RExpr -> [(InfixOp, RExpr)] -> RExpr
semanticInfixChain first rest =
    groupByPrecedence infixPrecedence ((Nothing, first):maybeRest)
  where
    maybeTup :: (InfixOp, RExpr) -> (Maybe InfixOp, RExpr)
    maybeTup (op, expr) = (Just op, expr) 

    maybeRest :: [(Maybe InfixOp, RExpr)]
    maybeRest = maybeTup <$> rest

semanticInfix :: PExpr -> InfixOp -> PExpr -> ParseExcept RExpr
semanticInfix lhs op rhs = do
  semanticLhs <- semantic lhs
  semanticRhs <- infixFlatten op rhs
  pure $ semanticInfixChain semanticLhs semanticRhs

infixFlatten :: InfixOp -> PExpr -> ParseExcept [(InfixOp, RExpr)]
infixFlatten op (Pos p (ExprInfix lhs nextOp rhs)) = do
  semanticLhs  <- semantic lhs
  semanticRest <- infixFlatten nextOp rhs
  pure $ (op, semanticLhs):semanticRest
infixFlatten op rhs = do
  semanticRhs  <- semantic rhs
  pure [(op, semanticRhs)]

semanticUnwrap :: [PExpr] -> ParseExcept RExpr
semanticUnwrap [] = throwError $ ErrString "Empty unwrap body"
semanticUnwrap [(Pos p (ExprBind _ _))] =
  throwError $ ErrPos p "Cannot have bind on last line of unwrap"
semanticUnwrap [last] = semantic last
semanticUnwrap ((Pos p (ExprBind arg expr)):xs) = do
  recursive    <- semanticUnwrap xs
  semanticExpr <- semantic expr

  pure $ RExprCall p
    (RExprId p "bind")
    [semanticExpr, RExprDef p [arg] [addReturn recursive]]
semanticUnwrap ((Pos p _):_) =
  throwError $ ErrPos p "All non-tails in unwrap must be binds"

addReturn :: RExpr -> RExpr
addReturn e = RExprReturn (rpos e) e

semanticCondBlock :: CondBlock PExpr -> ParseExcept (CondBlock RExpr)
semanticCondBlock (CondBlock cond body) = do
  newCond <- semantic cond
  newBody <- sequence $ semantic <$> body
  pure $ CondBlock newCond newBody

semanticCaseBlock :: CaseBlock PExpr -> ParseExcept (CaseBlock RExpr)
semanticCaseBlock (CaseBlock p arg body) = do
  newBody <- sequence $ semantic <$> body
  pure $ CaseBlock p arg newBody

semantic :: PExpr -> ParseExcept RExpr
-- Actual semantic alterations
semantic (Pos p (ExprUnwrap body)) = do
  --semanticBodies <- sequence $ semantic <$> body
  semanticUnwrap body

semantic (Pos p (ExprInfix lhs op rhs)) =
  semanticInfix lhs op rhs

-- Traversals
semantic (Pos p (ExprPrefixOp op ex)) = do
  semanticEx <- semantic ex
  pure $ RExprPrefixOp p op semanticEx
semantic (Pos _ (ExprPrecedence inner)) =
  semantic inner
semantic (Pos p (ExprIfElse ifCond elifConds elseBody)) = do
  newIfCond    <- semanticCondBlock ifCond 
  newElifConds <- sequence $ semanticCondBlock <$> elifConds 
  newElseBody  <- sequence $ semantic          <$> elseBody
  pure $ RExprIfElse p newIfCond newElifConds newElseBody
semantic (Pos p (ExprAssignment arg value)) = do
  newValue <- semantic value
  pure $ RExprAssignment p arg newValue
semantic (Pos p (ExprDef args body)) = do
  newBody <- sequence $ semantic <$> body
  pure $ RExprDef p args newBody
semantic (Pos p (ExprCall func params)) = do
  newFunc <- semantic func 
  newParams <- sequence $ semantic <$> params 
  pure $ RExprCall p newFunc newParams
semantic (Pos p (ExprReturn retVal)) = do
  newRetVal <- semantic retVal 
  pure $ RExprReturn p newRetVal
semantic (Pos p (ExprList elements)) = do
  newElements <- sequence $ semantic <$> elements 
  pure $ RExprList p newElements
semantic (Pos p (ExprTuple elements)) = do
  newElements <- sequence $ semantic <$> elements 
  pure $ RExprTuple p newElements
semantic (Pos p (ExprInstanceOf tname cname elements)) = do
  newElements <- sequence $ semantic <$> elements 
  pure $ RExprInstanceOf p tname cname newElements
semantic (Pos p (ExprCase input blocks)) = do
  newInput <- semantic input
  newBlocks <- sequence $ semanticCaseBlock <$> blocks
  pure $ RExprCase p newInput newBlocks
-- One to one maps
semantic (Pos p (ExprClass cname typeCons)) = do
  pure $ RExprClass p cname typeCons
semantic (Pos p (ExprType tname defSigs)) = do
  pure $ RExprType p tname defSigs
semantic (Pos p (ExprId name)) = do
  pure $ RExprId p name
semantic (Pos p (ExprChar value)) = do
  pure $ RExprChar p value
semantic (Pos p (ExprInt value)) = do
  pure $ RExprInt p value

semantic (Pos p other) = throwError $ ErrPos p $
  "Unexpected expr in semnatic: " <> show other
