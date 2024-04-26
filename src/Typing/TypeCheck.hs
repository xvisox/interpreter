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
                            else return $ insertVar ident varType env
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
typeCheckItem varType (NoInit pos ident) = declareIdentOrThrow pos ident varType
typeCheckItem varType (Init pos ident expr) = do
  exprType <- typeCheckExpr expr
  unless (exprType == varType) $ throwTypeCheckError pos $ TypeMismatch varType exprType
  declareIdentOrThrow pos ident varType

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
        exprKind <- if isVarExpr expr then return TCRef else return TCVal
        when (argType /= exprType || (argKind /= exprKind && argKind /= TCVal)) $
          throwTypeCheckError pos $ ArgumentMismatch (TCArgType argKind argType) (TCArgType exprKind exprType)
      return returnType
    Just _ -> throwTypeCheckError pos $ NotAFunction ident
    Nothing -> throwTypeCheckError pos $ NoVariable ident

isVarExpr :: Expr -> Bool
isVarExpr (EVar _ _) = True
isVarExpr _ = False

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
typeCheckStmt (BStmt _ block) = typeCheckBlock block >> ask
typeCheckStmt (DStmt _ topDef) = typeCheckTopDef topDef
typeCheckStmt (Ass pos ident expr) = typeCheckExpr expr >>= validateIdentOrThrow pos ident
typeCheckStmt (Incr pos ident) = validateIdentOrThrow pos ident TCInt
typeCheckStmt (Decr pos ident) = validateIdentOrThrow pos ident TCInt
typeCheckStmt (Ret pos expr) = do
  env <- ask
  exprType <- typeCheckExpr expr
  unless (exprType == returnType env) $ throwTypeCheckError pos $ TypeMismatch (returnType env) exprType
  return env { hasReturn = True }
typeCheckStmt (VRet pos) = do
  env <- ask
  unless (returnType env == TCVoid) $ throwTypeCheckError pos $ TypeMismatch TCVoid (returnType env)
  return env { hasReturn = True }
typeCheckStmt (Cond pos expr block) = do
  exprType <- typeCheckExpr expr
  unless (exprType == TCBool) $ throwTypeCheckError pos $ TypeMismatch TCBool exprType
  typeCheckBlock block >> ask
typeCheckStmt _ = do
  env <- ask
  throwTypeCheckError Nothing $ WrongArgumentCount 59599 $ scope env
