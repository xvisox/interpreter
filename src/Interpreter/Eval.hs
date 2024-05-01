module Interpreter.Eval where

import AbsSeeemcrd
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Common
import Interpreter.Environment
import Interpreter.Exception

-- Interpreter monad

type IM = ReaderT Env (ExceptT RuntimeError (StateT Store IO))

throwRuntimeError :: Pos -> RuntimeException -> IM a
throwRuntimeError pos err = throwError $ RuntimeError pos err

-- Utility functions

declareIdent :: Ident -> IVal -> IM Env
declareIdent ident value = do
  env <- ask
  store <- get

  let loc = newLoc store
  put $ insertLoc loc value store
  return $ insertNewVar ident loc env

lookupIdent :: Ident -> IM IVal
lookupIdent ident = do
  env <- ask
  store <- get

  let loc = lookupVar ident env
  let value = lookupLoc loc store
  return value

-- Interpreter functions

evaluate :: Program -> IO (Either RuntimeError (), Store)
evaluate program = runStateT (runExceptT (runReaderT (evalProgram program) initEnv)) initStore

evalProgram :: Program -> IM ()
evalProgram (PProgram _ topDefs) = do
  env <- evalTopDefs topDefs
  local (const env) $ evalExpr (EApp Nothing (Ident "main") []) >> return ()

evalTopDefs :: [TopDef] -> IM Env
evalTopDefs [] = ask
evalTopDefs (topDef:topDefs) = do
  env <- evalTopDef topDef
  local (const env) (evalTopDefs topDefs)

evalTopDef :: TopDef -> IM Env
evalTopDef (GlobalDef _ varType items) = evalItems varType items
evalTopDef (FnDef _ _ ident args block) = do
  let argsIdents = map (\(AArg _ _ argIdent) -> argIdent) args
  let argsKinds = map (\(AArg _ argKind _) -> case argKind of
        ValArg _ _ -> IArgVal
        RefArg _ _ -> IArgRef) args
  env <- ask
  declareIdent ident $ IFunc (zip argsKinds argsIdents) block env

evalItems :: Type -> [Item] -> IM Env
evalItems _ [] = ask
evalItems varType (item:items) = do
  env <- evalItem varType item
  local (const env) (evalItems varType items)

evalItem :: Type -> Item -> IM Env
evalItem varType (NoInit _ ident) = declareIdent ident (mapToDefaultIVal varType)
evalItem _ (Init _ ident expr) = evalExpr expr >>= declareIdent ident

evalExpr :: Expr -> IM IVal
evalExpr (EVar _ ident) = lookupIdent ident
evalExpr (ELitInt _ int) = return $ IInt (fromIntegral int)
evalExpr (ELitTrue _) = return $ IBool True
evalExpr (ELitFalse _) = return $ IBool False
evalExpr (EString _ string) = return $ IString string

evalExpr (Neg _ expr) = do
  IInt int <- evalExpr expr
  return $ IInt (negate int)

evalExpr (Not _ expr) = do
  IBool bool <- evalExpr expr
  return $ IBool (not bool)

evalExpr (EMul pos expr1 op expr2) = do
  IInt int1 <- evalExpr expr1
  IInt int2 <- evalExpr expr2
  case op of
    Times _ -> return $ IInt (int1 * int2)
    Div _ -> if int2 == 0
      then throwRuntimeError pos DivisionByZero
      else return $ IInt (int1 `div` int2)
    Mod _ -> if int2 == 0
      then throwRuntimeError pos DivisionByZero
      else return $ IInt (int1 `mod` int2)

evalExpr (EAdd pos expr1 op expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case (val1, val2, op) of
    (IInt int1, IInt int2, Plus _) -> return $ IInt (int1 + int2)
    (IInt int1, IInt int2, Minus _) -> return $ IInt (int1 - int2)
    (IString str1, IString str2, Plus _) -> return $ IString (str1 ++ str2)
    _ -> throwRuntimeError pos UnexpectedError

evalExpr (ERel _ expr1 op expr2) = do
  IInt int1 <- evalExpr expr1
  IInt int2 <- evalExpr expr2
  case op of
    LTH _ -> return $ IBool (int1 < int2)
    LE _ -> return $ IBool (int1 <= int2)
    GTH _ -> return $ IBool (int1 > int2)
    GE _ -> return $ IBool (int1 >= int2)
    EQU _ -> return $ IBool (int1 == int2)
    NE _ -> return $ IBool (int1 /= int2)

evalExpr (EAnd _ expr1 expr2) = do
  IBool bool1 <- evalExpr expr1
  IBool bool2 <- evalExpr expr2
  return $ IBool (bool1 && bool2)

evalExpr (EOr _ expr1 expr2) = do
  IBool bool1 <- evalExpr expr1
  IBool bool2 <- evalExpr expr2
  return $ IBool (bool1 || bool2)

evalExpr (ELambda _ _ args block) = do
  let argsIdents = map (\(AArg _ _ argIdent) -> argIdent) args
  let argsKinds = map (\(AArg _ argKind _) -> case argKind of
        ValArg _ _ -> IArgVal
        RefArg _ _ -> IArgRef) args
  env <- ask
  return $ IFunc (zip argsKinds argsIdents) block env

evalExpr (EApp pos ident exprs) = do
  if isBuiltInFunction ident
    then evalBuiltInFunction ident =<< mapM evalExpr exprs
    else do
      callEnv <- ask
      IFunc args block closure <- lookupIdent ident

      env <- foldM (\env (arg, expr) -> do
        case arg of
          (IArgVal, argIdent) -> do
            value <- evalExpr expr
            store <- get
            let loc = newLoc store
            put $ insertLoc loc value store
            return $ insertNewVar argIdent loc env
          (IArgRef, argIdent) -> case expr of
            EVar _ var -> do
              let loc = lookupVar var callEnv
              return $ insertNewVar argIdent loc env
            _ -> throwRuntimeError pos UnexpectedError
        ) closure $ zip args exprs

      local (const env) $ (evalBlock block >> return IVoid) `catchError` (\err -> case err of
        RuntimeError _ (ReturnFlag value) -> return value
        _ -> throwError err)

evalBlock :: Block -> IM Env
evalBlock (BBlock _ stmts) = do
  env <- ask
  local (const env) $ evalStmts stmts

evalStmts :: [Stmt] -> IM Env
evalStmts [] = ask
evalStmts (stmt:stmts) = do
  env <- evalStmt stmt
  local (const env) $ evalStmts stmts

evalStmt :: Stmt -> IM Env
evalStmt (BStmt _ block) = evalBlock block >> ask
evalStmt (DStmt _ topDef) = evalTopDef topDef
evalStmt (Ass _ ident expr) = do
  value <- evalExpr expr
  env <- ask
  let loc = lookupVar ident env

  store <- get
  put $ insertLoc loc value store
  return env

evalStmt (Incr pos ident) = do
  env <- ask
  let loc = lookupVar ident env

  store <- get
  case lookupLoc loc store of
    IInt value -> do
      let newValue = IInt (value + 1)
      put $ insertLoc loc newValue store
      return env
    _ -> throwRuntimeError pos UnexpectedError

evalStmt (Decr pos ident) = do
  env <- ask
  let loc = lookupVar ident env

  store <- get
  case lookupLoc loc store of
    IInt value -> do
      let newValue = IInt (value - 1)
      put $ insertLoc loc newValue store
      return env
    _ -> throwRuntimeError pos UnexpectedError

evalStmt (Ret pos expr) = do
  value <- evalExpr expr
  throwRuntimeError pos (ReturnFlag value)

evalStmt (VRet pos) = throwRuntimeError pos (ReturnFlag IVoid)

evalStmt (Cond _ expr block) = do
  IBool bool <- evalExpr expr
  if bool
    then evalBlock block >> ask
    else ask

evalStmt (CondElse _ expr ifBlock elseBlock) = do
  IBool bool <- evalExpr expr
  if bool
    then evalBlock ifBlock >> ask
    else evalBlock elseBlock >> ask

evalStmt (While _ expr block) = do
  IBool bool <- evalExpr expr
  if bool
    then evalBlock block >> evalStmt (While Nothing expr block)
    else ask

evalStmt (SExp _ expr) = evalExpr expr >> ask

evalBuiltInFunction :: Ident -> [IVal] -> IM IVal
evalBuiltInFunction (Ident "printStr") [IString string] = liftIO $ putStrLn string >> return IVoid
evalBuiltInFunction (Ident "printInt") [IInt int] = liftIO $ print int >> return IVoid
evalBuiltInFunction (Ident "printBool") [IBool bool] = liftIO $ print bool >> return IVoid
evalBuiltInFunction (Ident "toStr") [IInt int] = return $ IString (show int)
evalBuiltInFunction (Ident "toInt") [IString string] = case reads string of
  [(int, "")] -> return $ IInt int
  _ -> throwRuntimeError Nothing UnexpectedError
evalBuiltInFunction _ _ = throwRuntimeError Nothing UnexpectedError
