module Typing.TypeCheck where

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
throwTypeCheckError pos err = throwError $ TypeCheckError pos err

-- Utility functions

declareIdentOrThrow :: Pos -> Ident -> TCType -> TCM Env
declareIdentOrThrow pos ident varType = do
  env <- ask
  case lookupVar ident env of
    Just (_, varScope) -> if varScope == (scope env)
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

ensureType :: Pos -> TCType -> Expr -> TCM TCType
ensureType pos expectedType expr = do
  actualType <- typeCheckExpr expr
  unless (actualType == expectedType) $ throwTypeCheckError pos $ TypeMismatch expectedType actualType
  return actualType

-- Typechecking functions

typeCheck :: Program -> Either TypeCheckError ()
typeCheck (PProgram _ topDefs) = runExcept $ do
  env <- runReaderT (typeCheckTopDefs topDefs) initEnvWithBuiltins
  unless (hasValidMain env) $
    throwError $ TypeCheckError Nothing MainFunctionMissing

hasValidMain :: Env -> Bool
hasValidMain env = case lookupVar (Ident "main") env of
  Just (TCFun [] TCVoid, _) -> True
  _ -> False

typeCheckTopDefs :: [TopDef] -> TCM Env
typeCheckTopDefs [] = ask
typeCheckTopDefs (topDef:topDefs) = do
  env <- typeCheckTopDef topDef
  local (const env) $ typeCheckTopDefs topDefs

typeCheckTopDef :: TopDef -> TCM Env
typeCheckTopDef (GlobalDef _ varType items) = typeCheckItems (mapToTCType varType) items
typeCheckTopDef (FnDef pos returnType ident args block) = do
  let returnType' = mapToTCType returnType
  let argsTypes' = mapToTCArgTypes args
  env <- declareIdentOrThrow pos ident (TCFun argsTypes' returnType')

  let argsWithTypes' = mapToTypesWithIdents args
  env' <- foldM (\acc (type', ident') -> local (const acc) (declareIdentOrThrow pos ident' type')) (newScope env) argsWithTypes'
  env'' <- local (const env' { hasReturn = False, returnType = returnType', scope = scope env }) $ typeCheckBlock block

  unless (hasReturn env'' || returnType' == TCVoid) $ throwTypeCheckError pos $ NoReturn ident
  return env

typeCheckItems :: TCType -> [Item] -> TCM Env
typeCheckItems _ [] = ask
typeCheckItems varType (item:items) = do
  env <- typeCheckItem varType item
  local (const env) $ typeCheckItems varType items

typeCheckItem :: TCType -> Item -> TCM Env
typeCheckItem (TCFun _ _) (NoInit pos ident) = throwTypeCheckError pos $ FunctionNotDefined ident
typeCheckItem varType (NoInit pos ident) = declareIdentOrThrow pos ident varType
typeCheckItem varType (Init pos ident expr) = do
  exprType <- typeCheckExpr expr
  unless (exprType == varType) $ throwTypeCheckError pos $ TypeMismatch varType exprType
  declareIdentOrThrow pos ident varType

typeCheckExpr :: Expr -> TCM TCType
typeCheckExpr (ELitInt _ _) = return TCInt
typeCheckExpr (ELitTrue _) = return TCBool
typeCheckExpr (ELitFalse _) = return TCBool
typeCheckExpr (EString _ _) = return TCString
typeCheckExpr (Neg pos expr) = ensureType pos TCInt expr >> return TCInt
typeCheckExpr (Not pos expr) = ensureType pos TCBool expr >> return TCBool

typeCheckExpr (EVar pos ident) = do
  env <- ask
  case lookupVar ident env of
    Just (varType, _) -> return varType
    Nothing -> throwTypeCheckError pos $ NoVariable ident

typeCheckExpr (EMul pos expr1 _ expr2) = ensureType pos TCInt expr1 >> ensureType pos TCInt expr2 >> return TCInt

typeCheckExpr (EAdd pos expr1 _ expr2) = do
  exprType1 <- typeCheckExpr expr1
  exprType2 <- typeCheckExpr expr2
  case (exprType1, exprType2) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> return TCString
    _ -> throwTypeCheckError pos $ TypeMismatch exprType1 exprType2

typeCheckExpr (ERel pos expr1 _ expr2) = ensureType pos TCInt expr1 >> ensureType pos TCInt expr2 >> return TCBool

typeCheckExpr (EAnd pos expr1 expr2) = ensureType pos TCBool expr1 >> ensureType pos TCBool expr2 >> return TCBool

typeCheckExpr (EOr pos expr1 expr2) = ensureType pos TCBool expr1 >> ensureType pos TCBool expr2 >> return TCBool

typeCheckExpr (ELambda pos returnType args block) = do
  let returnType' = mapToTCType returnType
  let argsTypes' = mapToTCArgTypes args
  env <- ask

  let argsWithTypes' = mapToTypesWithIdents args
  env' <- foldM (\acc (type', ident') -> local (const acc) (declareIdentOrThrow pos ident' type')) (newScope env) argsWithTypes'
  env'' <- local (const env' { hasReturn = False, returnType = returnType', scope = scope env }) $ typeCheckBlock block

  unless (hasReturn env'' || returnType' == TCVoid) $ throwTypeCheckError pos $ NoReturn (Ident "lambda")
  return $ TCFun argsTypes' returnType'

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
typeCheckStmt (SExp _ expr) = typeCheckExpr expr >> ask

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

typeCheckStmt (CondElse pos expr ifBlock elseBlock) = do
  exprType <- typeCheckExpr expr
  unless (exprType == TCBool) $ throwTypeCheckError pos $ TypeMismatch TCBool exprType

  env <- ask
  ifEnv <- local (const env) $ typeCheckBlock ifBlock
  elseEnv <- local (const env) $ typeCheckBlock elseBlock
  return $ if hasReturn ifEnv && hasReturn elseEnv
           then env { hasReturn = True }
           else env { hasReturn = hasReturn env }

typeCheckStmt (While pos expr block) = do
  exprType <- typeCheckExpr expr
  unless (exprType == TCBool) $ throwTypeCheckError pos $ TypeMismatch TCBool exprType
  typeCheckBlock block >> ask
