module Interpreter.Exception where

import Common
import Interpreter.Environment

data RuntimeError = RuntimeError Pos RuntimeException

instance Show RuntimeError where
  show (RuntimeError pos ex) = show ex ++ " at " ++ showPos pos

data RuntimeException
  = DivisionByZero
  | IntegerParseError String
  | UnexpectedError
  | ReturnFlag IVal

instance Show RuntimeException where
  show DivisionByZero = "Division by zero"
  show UnexpectedError = "Unexpected error"
  show (IntegerParseError str) = "Cannot parse integer: " ++ str
  show (ReturnFlag val) = "Return " ++ show val
