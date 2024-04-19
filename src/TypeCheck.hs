module TypeCheck where

import Data.Map
import AbsSeeemcrd
import Control.Monad.Except
import Control.Monad.Reader

-- Types definition

data TCArgKind
  = TCRef
  | TCVal
  deriving (Eq, Read)

data TCType
  = TCFun [TCArgType] TCType
  | TCInt
  | TCBool
  | TCString
  | TCVoid
  deriving (Eq, Read)

data TCArgType = TCArgType TCArgKind TCType
  deriving (Eq, Read)

-- Environment, exceptions and monad definition

data Env = Env {
  variables :: Map Ident TCType,
  hasReturn :: Bool,
  returnType :: TCType
}

data TypeCheckException
  = AssignmentMismatch
  | Redeclared Ident

instance Show TypeCheckException where
  show AssignmentMismatch = "Assignment mismatch"
  show (Redeclared ident) = "Redeclared variable: " ++ show ident

type TCM = ReaderT Env (Except TypeCheckException)

-- Typechecking entrypoint

typeCheck :: Program -> Either TypeCheckException ()
typeCheck (PProgram _ topDefs) = runExcept $ runReaderT (typeCheckTopDefs topDefs) (Env empty False TCVoid)

-- Typechecking functions

typeCheckTopDefs :: [TopDef] -> TCM ()
typeCheckTopDefs [] = return ()
typeCheckTopDefs (topDef:topDefs) = do
  typeCheckTopDef topDef
  typeCheckTopDefs topDefs

typeCheckTopDef :: TopDef -> TCM ()
typeCheckTopDef (GlobalDef _ _ items) = do -- TODO: Implement
  return ()