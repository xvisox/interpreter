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

data TypingException
  = AssignmentMismatch
  | Redeclared Ident

instance Show TypingException where
  show AssignmentMismatch = "Assignment mismatch"
  show (Redeclared ident) = "Redeclared variable: " ++ show ident

type TCM = ReaderT Env (Except TypingException)

-- Typechecking entrypoint

typeCheck :: Program -> Either TypingException ()
typeCheck _ = Right ()
