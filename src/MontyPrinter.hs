{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module MontyPrinter where

import MyPrelude

import qualified Data.HashMap.Strict as HM
import MiddleEndTypes
import PrettyPrint
import Control.Monad.State (runState)
import ParserTypes (Id, Arg (IdArg, _idArgVal))
import Control.Monad.Except

addFunc :: Env a -> FuncDef a -> Env a
addFunc env funcDef =
  over funcTable (HM.insert (env ^. latestFuncId) funcDef) env &
  over latestFuncId (1+)

addConstant :: forall a. Env a -> Id -> MExpr a -> Except String (Env a)
addConstant env id expr =
  case env ^. (idTable . at id) of
    Just _ -> throwError [i|#{id} is already defined|]
    _      -> pure $ over idTable (HM.insert id exprWithId) envWithFunc
  where
    envWithFunc :: Env a
    envWithFunc = case exprWithId of
      e@MExprDef {} -> addFunc env $ defToFuncDef (Just id) e
      _             -> env

    exprWithId :: MExpr a
    exprWithId = case expr of
      -- FIXME: e@MExprDef {} -> expr & funcId ?~ (env ^. latestFuncId)
      _             -> expr

-- FIXME: Once we move to GADTs (again) this rubbish can go away
defToFuncDef :: Maybe Id -> MExpr a -> FuncDef a
defToFuncDef id expr@MExprDef { _args } =
  FuncDef id (Just $ expr ^. mpos) (length _args) [] (Just expr)
  where
    funcId = env ^. latestFuncId
defToFuncDef _ _ = undefined

inferMeDaddy :: forall a. Env a -> MExpr a -> Except String (Env a)
inferMeDaddy env expr@MExprDef {} =
  pure $ addFunc env $ defToFuncDef Nothing expr
inferMeDaddy env expr@MExprAssignment { _lhs = IdArg { _idArgVal }, _rhs } =
  addConstant env _idArgVal _rhs
inferMeDaddy env _ = pure env

env = foldl addFunc emptyEnv [
    FuncDef (Just "#lessequal") Nothing 2 [
      FuncSig [MInt, MInt] MInt,
      FuncSig [MDouble, MDouble] MDouble
    ] Nothing,
    FuncDef (Just "#add") Nothing 2 [
      FuncSig [MInt, MInt] MInt,
      FuncSig [MDouble, MDouble] MDouble
    ] Nothing,
    FuncDef (Just "#subtract") Nothing 2 [
      FuncSig [MInt, MInt] MInt,
      FuncSig [MDouble, MDouble] MDouble
    ] Nothing
  ]

run :: [MExpr a] -> IO ()
run prog = do
  putStrLn $ intercalate "\n" (show <$> prog)

  case runExcept $ foldM inferMeDaddy env prog of
    Right env  -> print env
    Left error -> print error

  pure ()
