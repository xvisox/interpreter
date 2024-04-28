module Interpreter.Exception where

import Prelude
import Common

data RuntimeError = RuntimeError Pos RuntimeException

instance Show RuntimeError where
  show (RuntimeError pos ex) = show ex ++ " at " ++ showPos pos

data RuntimeException
  = DivisionByZero
  | UndefinedVariable

instance Show RuntimeException where
  show DivisionByZero = "Division by zero"
  show UndefinedVariable = "Undefined variable"
