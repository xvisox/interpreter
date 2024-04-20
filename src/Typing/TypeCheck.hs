module Typing.TypeCheck where

import Data.Map
import AbsSeeemcrd
import Control.Monad.Except
import Control.Monad.Reader

import Common
import Typing.Types
import Typing.Exception
import Typing.Environment

type TCM = ReaderT Env (Except TypeCheckException)

typeCheck :: Program -> Either TypeCheckException ()
typeCheck (PProgram _ topDefs) = runExcept $ runReaderT (typeCheckTopDefs topDefs) (Env empty False TCVoid)

-- Typechecking functions

typeCheckTopDefs :: [TopDef] -> TCM ()
typeCheckTopDefs [] = return ()
typeCheckTopDefs (topDef:topDefs) = do
  typeCheckTopDef topDef
  typeCheckTopDefs topDefs

typeCheckTopDef :: TopDef -> TCM ()
typeCheckTopDef (GlobalDef _ varType items) = do
  env <- ask
  let varTCType = mapToTCType varType
  local (\env -> env { variables = Data.Map.empty }) $ typeCheckItems varTCType items

typeCheckItems :: TCType -> [Item] -> TCM ()
typeCheckItems _ [] = return ()
typeCheckItems varType (item:items) = do
  typeCheckItem varType item
  typeCheckItems varType items

typeCheckItem :: TCType -> Item -> TCM ()
typeCheckItem varType (NoInit _ ident) = do
  env <- ask
  if Data.Map.member ident (variables env)
    then throwError $ Redeclared ident
    else local (\env -> env { variables = Data.Map.insert ident varType (variables env) }) $ return ()
typeCheckItem varType (Init pos ident expr) = do
  env <- ask
  exprType <- typeCheckExpr expr
  if exprType == varType
    then local (\env -> env { variables = Data.Map.insert ident varType (variables env) }) $ return ()
    else throwError $ AssignmentMismatch varType exprType pos

typeCheckExpr :: Expr -> TCM TCType
typeCheckExpr (EVar _ ident) = do
  env <- ask
  case Data.Map.lookup ident (variables env) of
    Just varType -> return varType
    Nothing -> throwError $ NoVariable ident
typeCheckExpr (ELitInt _ _) = return TCInt
typeCheckExpr (ELitTrue _) = return TCBool
