module Parser.Semantic (ParseErr(..), ParseExcept, semantic) where

import Data.Maybe
import Data.Void
import System.Exit
import Text.Megaparsec hiding (Pos)
import Control.Monad.Except

import ParserTypes
import RunnerUtils
import CallableUtils

data ParseErr
  = ErrPos SourcePos String
  | ErrString String
  | ErrParse (ParseErrorBundle String Void)

instance Show ParseErr where
  show (ErrParse bundle) = errorBundlePretty bundle
  show (ErrPos p message) = message <> " at " <> show p
  show (ErrString message) = message

type ParseExcept = Except ParseErr

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

-- Traversals
semantic (Pos p (ExprIfElse ifCond elifConds elseBody)) = do
  newIfCond    <- semanticCondBlock ifCond 
  newElifConds <- sequence $ semanticCondBlock <$> elifConds 
  newElseBody  <- sequence $ semantic          <$> elseBody
  pure $ Pos p $ ExprIfElse newIfCond newElifConds newElseBody
semantic (Pos p (ExprInfix lhs op rhs)) = do
  newLhs <- semantic lhs
  newRhs <- semantic rhs
  pure $ Pos p $ ExprInfix newLhs op newRhs
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
