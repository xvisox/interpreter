module Interpreter.Eval where

import AbsSeeemcrd
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
evalProgram (PProgram _ topDefs) = evalTopDefs topDefs >> return ()

evalTopDefs :: [TopDef] -> IM Env
evalTopDefs [] = ask
evalTopDefs (topDef:topDefs) = do
  env <- evalTopDef topDef
  local (const env) (evalTopDefs topDefs)

evalTopDef :: TopDef -> IM Env
evalTopDef (GlobalDef _ varType items) = evalItems varType items
evalTopDef _ = ask -- TODO

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

evalExpr (EAdd _ expr1 op expr2) = do
  IInt int1 <- evalExpr expr1
  IInt int2 <- evalExpr expr2
  case op of
    Plus _ -> return $ IInt (int1 + int2)
    Minus _ -> return $ IInt (int1 - int2)

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

evalExpr _ = return IVoid -- TODO
