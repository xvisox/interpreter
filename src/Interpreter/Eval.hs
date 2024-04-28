module Interpreter.Eval where

import Data.Map
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
throwRuntimeError pos error = throwError $ RuntimeError pos error

-- Utility functions

declareIdent :: Ident -> IVal -> IM Env
declareIdent ident value = do
  env <- ask
  store <- get

  let loc = newLoc store
  put $ insertLoc loc value store
  return $ insertNewVar ident loc env

lookupIdent :: Pos -> Ident -> IM IVal
lookupIdent pos ident = do
  env <- ask
  store <- get

  let Just loc = lookupVar ident env
  let Just value = lookupLoc loc store
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
evalTopDef (FnDef pos varType ident args block) = ask -- TODO

evalItems :: Type -> [Item] -> IM Env
evalItems _ [] = ask
evalItems varType (item:items) = do
  env <- evalItem varType item
  local (const env) (evalItems varType items)

evalItem :: Type -> Item -> IM Env
evalItem varType (NoInit _ ident) = declareIdent ident (mapToDefaultIVal varType)
evalItem varType (Init _ ident expr) = evalExpr expr >>= declareIdent ident

evalExpr :: Expr -> IM IVal
evalExpr _ = return IVoid
