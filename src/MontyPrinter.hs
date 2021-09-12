{-# LANGUAGE DataKinds #-}
module MontyPrinter where

import MyPrelude

import qualified Data.HashMap.Strict as HM
import PrettyPrint
import Control.Monad.State (runState)
import ParserTypes (Id, Arg (IdArg, _idArgVal))
import Control.Monad.Except
import MorphUtils
import MiddleEndTypes

addFunc :: Env -> FuncDef -> Env
addFunc env funcDef =
  over funcTable (HM.insert (env ^. latestFuncId) funcDef) env &
  over latestFuncId (1+)

addConstant :: Env -> Id -> ExistsMExpr -> Except String Env
addConstant env id (Exists expr) =
  case env ^. (idTable . at id) of
    Just _ -> throwError [i|#{id} is already defined|]
    _      -> pure $ over idTable (HM.insert id $ Exists exprWithId) envWithFunc
  where
    envWithFunc :: Env
    envWithFunc = case exprWithId of
      e@MExprDef {} -> addFunc env $ defToFuncDef (Just id) e
      _             -> env

    exprWithId = case expr of
      e@MExprDef {} -> expr & funcId ?~ (env ^. latestFuncId)
      _             -> expr

defToFuncDef :: Maybe Id -> MExpr MExprDefType -> FuncDef
defToFuncDef id expr@MExprDef { _defMpos, _args } =
  FuncDef id (Just _defMpos) (length _args) [] (Just $ Exists expr)
  where
    funcId = env ^. latestFuncId

inferMeDaddy :: Env -> ExistsMExpr  -> Except String Env
inferMeDaddy env (Exists expr@MExprDef {}) =
  pure $ addFunc env $ defToFuncDef Nothing expr
inferMeDaddy env (Exists expr@MExprAssignment { _lhs = IdArg { _idArgVal }, _rhs }) =
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

run :: [ExistsMExpr] -> IO ()
run prog = do
  putStrLn $ intercalate "\n" (show <$> prog)

  case runExcept $ foldM inferMeDaddy env prog of
    Right env  -> print env
    Left error -> print error

  pure ()
