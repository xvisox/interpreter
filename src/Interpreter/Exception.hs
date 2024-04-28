module Interpreter.Exception where

data RuntimeException
  = DivisionByZero
  | VariableNotFound
  | FunctionNotFound
  | WrongArgumentCount Int Int
  | ArgumentMismatch String String
  | NoReturn
  | MainFunctionMissing

instance Show RuntimeException where
  show DivisionByZero = "Division by zero"
  show VariableNotFound = "Variable not found"
  show FunctionNotFound = "Function not found"
  show (WrongArgumentCount expected got) = "Wrong argument count: expected " ++ show expected ++ ", got " ++ show got
  show (ArgumentMismatch expected got) = "Argument mismatch: expected " ++ expected ++ ", got " ++ got
  show NoReturn = "No return statement"
  show MainFunctionMissing = "Main function missing"