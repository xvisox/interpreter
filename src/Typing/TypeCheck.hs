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
  let maybeVar = lookupVar ident env
  case maybeVar of
    Just (_, varScope) -> if varScope == currentScope
                            then throwTypeCheckError pos $ Redeclared ident
                            else return env -- Shadowing allowed, so the identifier is not redeclared
    Nothing -> return $ insertVar ident varType env

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
    else throwTypeCheckError pos $ AssignmentMismatch varType exprType

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
      if length args /= length exprs
        then throwTypeCheckError pos $ WrongArgumentCount (length args) (length exprs)
        else do
          let argExprPairs = zip args exprs
          forM_ argExprPairs $ \((TCArgType argKind argType), expr) -> do
            exprType <- typeCheckExpr expr
            exprKind <- case expr of
              EVar _ _ -> return TCRef
              _ -> return TCVal
            if argType == exprType && (argKind == exprKind || argKind == TCVal)
              then return ()
              else throwTypeCheckError pos $ ArgumentMismatch (TCArgType argKind argType) (TCArgType exprKind exprType)
          return returnType
    Just _ -> throwTypeCheckError pos $ NotAFunction ident
    Nothing -> throwTypeCheckError pos $ NoVariable ident
