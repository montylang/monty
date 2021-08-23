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

import Evaluators.All
import Evaluators.Import
import TypeUtils (pexprToTree)

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

-- What a mess
groupByPrecedence :: [InfixOp] -> [(Maybe InfixOp, ET)] -> ET
groupByPrecedence [] [] = trace "How did I get here?" undefined
groupByPrecedence [] [(_, x)] = x
groupByPrecedence [] ((Just op, x):xs) =
  ET $ RInfix (getPos x) x op $ groupByPrecedence [] xs
groupByPrecedence (o:os) xs = joinHeadOp subCases
  where
    subCases :: [ET]
    subCases = groupByPrecedence os <$>
      multiSpan ((== Just o) . view _1) xs

    joinHeadOp :: [ET] -> ET
    joinHeadOp [y] = y
    joinHeadOp (y:ys) = foldl folderHeadOp y ys

    folderHeadOp :: ET -> ET -> ET
    folderHeadOp acc it = ET $ RInfix (getPos it) acc o it

semanticInfixChain :: ET -> [(InfixOp, ET)] -> ET
semanticInfixChain first rest =
    groupByPrecedence infixSplitOrder ((Nothing, first):maybeRest)
  where
    maybeTup :: (InfixOp, ET) -> (Maybe InfixOp, ET)
    maybeTup (op, expr) = (Just op, expr) 

    maybeRest :: [(Maybe InfixOp, ET)]
    maybeRest = maybeTup <$> rest

semanticInfix :: PExpr -> InfixOp -> PExpr -> ParseExcept ET
semanticInfix lhs op rhs = do
  semanticLhs <- semantic lhs
  semanticRhs <- infixFlatten op rhs
  pure $ semanticInfixChain semanticLhs semanticRhs

infixFlatten :: InfixOp -> PExpr -> ParseExcept [(InfixOp, ET)]
infixFlatten op (Pos p (ExprInfix lhs nextOp rhs)) = do
  semanticLhs  <- semantic lhs
  semanticRest <- infixFlatten nextOp rhs
  pure $ (op, semanticLhs):semanticRest
infixFlatten op rhs = do
  semanticRhs  <- semantic rhs
  pure [(op, semanticRhs)]

semanticUnwrap :: [PExpr] -> ParseExcept ET
semanticUnwrap [] = throwError $ ErrString "Empty unwrap body"
semanticUnwrap [Pos p (ExprBind _ _)] =
  throwError $ ErrPos p "Cannot have bind on last line of unwrap"
semanticUnwrap [Pos p (ExprAssignment _ _)] =
  throwError $ ErrPos p "Cannot have assignment on last line of unwrap"
semanticUnwrap [last] = semantic last
semanticUnwrap ((Pos p (ExprBind arg expr)):xs) = do
  semanticExpr <- semantic expr
  recursive    <- semanticUnwrap xs

  pure $ ET $ RCall p
    (ET $ RId p "bind")
    [ET semanticExpr, ET $ RDef p Nothing [arg] [recursive]]
semanticUnwrap (expr@(Pos p (ExprAssignment _ _)):xs) = do
  semanticExpr <- semantic expr
  recursive    <- semanticUnwrap xs

  pure $ ET $ RBlock p [semanticExpr, recursive]
semanticUnwrap ((Pos p expr):xs) = do
  semanticUnwrap $ Pos p (ExprBind (IdArg "_") (Pos p expr)):xs

semanticCondBlock :: CondBlock PExpr -> ParseExcept (CondBlock ET)
semanticCondBlock (CondBlock cond body) = do
  newCond <- semantic cond
  newBody <- sequence $ semantic <$> body
  pure $ CondBlock newCond newBody

semanticCaseBlock :: CaseBlock PExpr -> ParseExcept (CaseBlock ET)
semanticCaseBlock (CaseBlock p arg body) = do
  newBody <- sequence $ semantic <$> body
  pure $ CaseBlock p arg newBody

semanticInstanceDef :: PExpr -> ParseExcept (RAssignment RDef)
semanticInstanceDef expr@(Pos _ (ExprAssignment (IdArg name) _)) =
  semanticAss (semanticDef $ Just name) expr

semanticAss :: Evaluatable a
  => (PExpr -> ParseExcept a)
  -> PExpr
  -> ParseExcept (RAssignment a)
semanticAss rhsSemantic (Pos p (ExprAssignment arg value)) = do
  rhs <- rhsSemantic value
  pure $ RAssignment p arg rhs
semanticAss _ (Pos p _) = throwError $ ErrPos p "Not an assignment"

rearrangeCond :: SourcePos
              -> CondBlock PExpr
              -> [CondBlock PExpr]
              -> [PExpr]
              -> ParseExcept ET
rearrangeCond p ifCond elifConds elseBody = do
  newIfCond    <- rearrangeCondBlock ifCond
  newElifConds <- sequence $ rearrangeCondBlock <$> elifConds
  newElseBody  <- rearrangeReturn elseBody
  pure $ ET $ RCondition p newIfCond newElifConds newElseBody

  where
    rearrangeCondBlock :: CondBlock PExpr -> ParseExcept (CondBlock ET)
    rearrangeCondBlock (CondBlock cond body) =
      liftM2 CondBlock (semantic cond) (rearrangeReturn body)

-- Rearranges a function such that the last statement will be a return
rearrangeReturn :: [PExpr] -> ParseExcept [ET]
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

semanticDef :: Maybe Id -> PExpr -> ParseExcept RDef
semanticDef name (Pos p (ExprDef args body)) = do
  --newBody <- sequence $ semantic <$> body
  newBody <- rearrangeReturn body
  pure $ RDef p name args newBody
semanticDef _ (Pos p _) = throwError $ ErrPos p "Not a def"

semantic :: PExpr -> ParseExcept ET
-- Actual semantic alterations
semantic (Pos p (ExprUnwrap body)) = do
  --semanticBodies <- sequence $ semantic <$> body
  semanticUnwrap body

semantic (Pos p (ExprInfix lhs op rhs)) =
  semanticInfix lhs op rhs

-- Traversals
semantic (Pos p (ExprPrefixOp op ex)) = do
  semanticEx <- semantic ex
  pure $ ET $ RPrefix p op semanticEx
semantic (Pos _ (ExprPrecedence inner)) =
  semantic inner
semantic (Pos p (ExprIfElse ifCond elifConds elseBody)) = do
  elseBody'    <- getOrDie elseBody
  newIfCond    <- semanticCondBlock ifCond
  newElifConds <- sequence $ semanticCondBlock <$> elifConds
  newElseBody  <- sequence $ semantic          <$> elseBody'
  pure $ ET $ RCondition p newIfCond newElifConds newElseBody
  where
    getOrDie :: Maybe [PExpr] -> ParseExcept [PExpr]
    getOrDie (Just xs) = pure xs
    getOrDie _ =
      throwError $ ErrPos p "Non-returning conditional must have an else clause"
semantic ass@(Pos _ (ExprAssignment arg@(IdArg name) value@(Pos _ (ExprDef _ _)))) = do
  ET <$> semanticAss (semanticDef $ Just name) ass
semantic ass@(Pos _ (ExprAssignment _ _)) = do
  ET <$> semanticAss semantic ass
semantic def@(Pos _ ExprDef {}) = do
  ET <$> semanticDef Nothing def
semantic (Pos p (ExprCall func params)) = do
  newFunc <- semantic func 
  newParams <- sequence $ semantic <$> params 
  pure $ ET $ RCall p newFunc newParams
semantic (Pos p (ExprList elements)) = do
  newElements <- sequence $ semantic <$> elements 
  pure $ ET $ RList p newElements
semantic (Pos p (ExprTuple elements)) = do
  newElements <- sequence $ semantic <$> elements 
  pure $ ET $ RTuple p newElements
semantic (Pos p (ExprInstanceOf tname cname elements)) = do
  newElements <- sequence $ semanticInstanceDef <$> elements
  pure $ ET $ RInstanceOf p tname cname newElements
semantic (Pos p (ExprCase input blocks)) = do
  newInput  <- semantic input
  newBlocks <- sequence $ semanticCaseBlock <$> blocks
  pure $ ET $ RCase p newInput newBlocks
-- One to one maps
semantic (Pos p (ExprImport path)) = do
  pure $ ET $ RImport p path
semantic (Pos p (ExprClass cname typeCons)) = do
  pure $ ET $ RClass p cname typeCons
semantic (Pos p (ExprType tname defSigs)) = do
  pure $ ET $ RType p tname defSigs
semantic (Pos p (ExprId name)) = do
  pure $ ET $ RId p name
semantic (Pos p (ExprChar value)) = do
  pure $ ET $ RChar p value
semantic (Pos p (ExprInt value)) = do
  pure $ ET $ RInt p value
semantic (Pos p (ExprDouble value)) = do
  pure $ ET $ RDouble p value

semantic (Pos p other) = throwError $ ErrPos p $
  "Unexpected expr in semnatic: " <> show other
