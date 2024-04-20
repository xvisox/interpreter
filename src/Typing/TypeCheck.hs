module Typing.TypeCheck where

import Data.Map
import AbsSeeemcrd
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad(void)

import Common
import Typing.Types
import Typing.Exception
import Typing.Environment

type TCM = ReaderT Env (Except TypeCheckException)

typeCheck :: Program -> Either TypeCheckException ()
typeCheck (PProgram _ topDefs) = runExcept $ runReaderT (void $ typeCheckTopDefs topDefs) initEnv

-- Typechecking functions

typeCheckTopDefs :: [TopDef] -> TCM Env
typeCheckTopDefs [] = ask
typeCheckTopDefs (topDef:topDefs) = do
  env <- typeCheckTopDef topDef
  local (const env) $ typeCheckTopDefs topDefs

typeCheckTopDef :: TopDef -> TCM Env
typeCheckTopDef (GlobalDef _ varType items) = do
  typeCheckItems (mapToTCType varType) items

typeCheckItems :: TCType -> [Item] -> TCM Env
typeCheckItems varType [] = ask
typeCheckItems varType (item:items) = do
  env <- typeCheckItem varType item
  local (const env) $ typeCheckItems varType items

typeCheckItem :: TCType -> Item -> TCM Env
typeCheckItem varType (NoInit _ ident) = do
  env <- ask
  if lookupVar ident env /= Nothing
    then throwError $ Redeclared ident
    else return $ insertVar ident varType env
typeCheckItem varType (Init pos ident expr) = do
  env <- ask
  exprType <- typeCheckExpr expr
  if exprType == varType
    then if lookupVar ident env /= Nothing
      then throwError $ Redeclared ident
      else return $ insertVar ident varType env
    else throwError $ AssignmentMismatch varType exprType pos

typeCheckExpr :: Expr -> TCM TCType
typeCheckExpr (EVar _ ident) = do
  env <- ask
  case lookupVar ident env of
    Just (varType, _) -> return varType
    Nothing -> throwError $ NoVariable ident
typeCheckExpr (ELitInt _ _) = return TCInt
typeCheckExpr (ELitTrue _) = return TCBool
typeCheckExpr (ELitFalse _) = return TCBool
typeCheckExpr (EString _ _) = return TCString
