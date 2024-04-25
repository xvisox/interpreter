module Typing.TypeCheck where

import Data.Map
import AbsSeeemcrd
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad

import Common
import Typing.Types
import Typing.Exception
import Typing.Environment

-- Typechecking monad

type TCM = ReaderT Env (Except TypeCheckError)

throwTypeCheckError :: Pos -> TypeCheckException -> TCM a
throwTypeCheckError pos error = throwError $ TypeCheckError pos error

declareIdentOrThrow :: Pos -> Ident -> TCType -> TCM Env
declareIdentOrThrow pos ident varType = do
  env <- ask
  let currentScope = scope env
  case lookupVar ident env of
    Just (_, varScope) -> if varScope == currentScope
                            then throwTypeCheckError pos $ Redeclared ident
                            else return env -- Shadowing allowed, so the identifier is not redeclared
    Nothing -> return $ insertVar ident varType env

validateIdentOrThrow :: Pos -> Ident -> TCType -> TCM Env
validateIdentOrThrow pos ident expectedType = do
  env <- ask
  case lookupVar ident env of
    Just (varType, _) -> if varType /= expectedType
                          then throwTypeCheckError pos $ TypeMismatch expectedType varType
                          else return env
    Nothing -> throwTypeCheckError pos $ NoVariable ident

-- Typechecking functions

typeCheck :: Program -> Either TypeCheckError ()
typeCheck (PProgram _ topDefs) = runExcept $ runReaderT (void $ typeCheckTopDefs topDefs) initEnv

typeCheckTopDefs :: [TopDef] -> TCM Env
typeCheckTopDefs [] = ask
typeCheckTopDefs (topDef:topDefs) = do
  env <- typeCheckTopDef topDef
  local (const env) $ typeCheckTopDefs topDefs

typeCheckTopDef :: TopDef -> TCM Env
typeCheckTopDef (GlobalDef _ varType items) = do
  typeCheckItems (mapToTCType varType) items
typeCheckTopDef (FnDef pos returnType ident args block) = do
  let returnType' = mapToTCType returnType
  env <- declareIdentOrThrow pos ident (TCFun (mapToTCArgTypes args) returnType')

  let typesWithIdents = mapToTypesWithIdents args
  let argEnv = Prelude.foldl (\acc (varType, ident) -> insertVar ident varType acc) env typesWithIdents
  env' <- local (const argEnv { hasReturn = False, returnType = returnType' }) $ typeCheckBlock block

  unless (hasReturn env') $ throwTypeCheckError pos $ NoReturn ident
  return env

typeCheckItems :: TCType -> [Item] -> TCM Env
typeCheckItems varType [] = ask
typeCheckItems varType (item:items) = do
  env <- typeCheckItem varType item
  local (const env) $ typeCheckItems varType items

typeCheckItem :: TCType -> Item -> TCM Env
typeCheckItem varType (NoInit pos ident) = do
  return =<< declareIdentOrThrow pos ident varType
typeCheckItem varType (Init pos ident expr) = do
  exprType <- typeCheckExpr expr
  if exprType == varType
    then return =<< declareIdentOrThrow pos ident varType
    else throwTypeCheckError pos $ TypeMismatch varType exprType

typeCheckExpr :: Expr -> TCM TCType
typeCheckExpr (EVar pos ident) = do
  env <- ask
  case lookupVar ident env of
    Just (varType, _) -> return varType
    Nothing -> throwTypeCheckError pos $ NoVariable ident
typeCheckExpr (ELitInt _ _) = return TCInt
typeCheckExpr (ELitTrue _) = return TCBool
typeCheckExpr (ELitFalse _) = return TCBool
typeCheckExpr (EString _ _) = return TCString
typeCheckExpr (EApp pos ident exprs) = do
  env <- ask
  case lookupVar ident env of
    Just (TCFun args returnType, _) -> do
      when (length args /= length exprs) $
        throwTypeCheckError pos $ WrongArgumentCount (length args) (length exprs)
      let argExprPairs = zip args exprs
      forM_ argExprPairs $ \((TCArgType argKind argType), expr) -> do
        exprType <- typeCheckExpr expr
        exprKind <- case expr of
          EVar _ _ -> return TCRef
          _ -> return TCVal
        when (argType /= exprType || (argKind /= exprKind && argKind /= TCVal)) $
          throwTypeCheckError pos $ ArgumentMismatch (TCArgType argKind argType) (TCArgType exprKind exprType)
      return returnType
    Just _ -> throwTypeCheckError pos $ NotAFunction ident
    Nothing -> throwTypeCheckError pos $ NoVariable ident

typeCheckBlock :: Block -> TCM Env
typeCheckBlock (BBlock _ stmts) = do
  env <- ask
  local (const $ newScope env) $ typeCheckStmts stmts

typeCheckStmts :: [Stmt] -> TCM Env
typeCheckStmts [] = ask
typeCheckStmts (stmt:stmts) = do
  env <- typeCheckStmt stmt
  local (const env) $ typeCheckStmts stmts

typeCheckStmt :: Stmt -> TCM Env
typeCheckStmt (BStmt _ block) = typeCheckBlock block
typeCheckStmt (DStmt _ topDef) = typeCheckTopDef topDef
typeCheckStmt (Ass pos ident expr) = do
  exprType <- typeCheckExpr expr
  validateIdentOrThrow pos ident exprType
typeCheckStmt (Incr pos ident) = validateIdentOrThrow pos ident TCInt
typeCheckStmt (Decr pos ident) = validateIdentOrThrow pos ident TCInt
typeCheckStmt (Ret pos expr) = do
  env <- ask
  exprType <- typeCheckExpr expr
  let expectedReturnType = returnType env
  if exprType == expectedReturnType
    then return env { hasReturn = True }
    else throwTypeCheckError pos $ TypeMismatch expectedReturnType exprType
typeCheckStmt (VRet pos) = do
  env <- ask
  let expectedReturnType = returnType env
  if expectedReturnType == TCVoid
    then return env { hasReturn = True }
    else throwTypeCheckError pos $ TypeMismatch TCVoid expectedReturnType
typeCheckStmt _ = throwTypeCheckError Nothing $ WrongArgumentCount 1 2
