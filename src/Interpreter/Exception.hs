module Interpreter.Exception where

import Prelude
import Interpreter.Environment
import Common

data RuntimeError = RuntimeError Pos RuntimeException

instance Show RuntimeError where
  show (RuntimeError pos ex) = show ex ++ " at " ++ showPos pos

data RuntimeException
  = DivisionByZero
  | UnexpectedError
  | ReturnFlag IVal

instance Show RuntimeException where
  show DivisionByZero = "Division by zero"
  show UnexpectedError = "Unexpected error"
  show (ReturnFlag val) = "Return " ++ show val
