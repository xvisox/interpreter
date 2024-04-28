module Interpreter.Eval where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import AbsSeeemcrd
import Interpreter.Environment
import Interpreter.Exception

-- Interpreter monad

type IM = ReaderT Env (ExceptT RuntimeException (StateT Store IO))

-- Interpreter functions

evaluate :: Program -> IO (Either RuntimeException (), Store)
evaluate program = runStateT (runExceptT (runReaderT (evalProgram program) initEnv)) initStore

evalProgram :: Program -> IM ()
evalProgram (PProgram _ topDefs) = do
  env <- evalTopDefs topDefs
  return ()

evalTopDefs :: [TopDef] -> IM Env
evalTopDefs _ = ask

evalTopDef :: TopDef -> IM Env
evalTopDef _ = ask
