module Typing.Exception where

import AbsSeeemcrd(Ident)

import Common
import Typing.Types

data TypeCheckError = TypeCheckError Pos TypeCheckException

instance Show TypeCheckError where
  show (TypeCheckError pos ex) = show ex ++ " at " ++ showPos pos

data TypeCheckException
  = TypeMismatch TCType TCType
  | Redeclared Ident
  | NoVariable Ident
  | NotAFunction Ident
  | WrongArgumentCount Int Int
  | ArgumentMismatch TCArgType TCArgType
  | NoReturn Ident
  | FunctionVarNotInitialized Ident
  | VoidInvalidType Ident
  | BuiltInFunctionOverride Ident
  | InvalidOperation TCType TCType
  | MainFunctionMissing

instance Show TypeCheckException where
  show (TypeMismatch expected actual) = "Type mismatch: expected " ++ show expected ++ ", got " ++ show actual
  show (Redeclared ident) = "Redeclared variable: " ++ show ident
  show (NoVariable ident) = "No such variable: " ++ show ident
  show (NotAFunction ident) = "Not a function: " ++ show ident
  show (WrongArgumentCount expected actual) = "Wrong argument count: expected " ++ show expected ++ ", got " ++ show actual
  show (ArgumentMismatch expected actual) = "Argument mismatch: expected " ++ show expected ++ ", got " ++ show actual
  show (NoReturn ident) = "No return statement in function: " ++ show ident
  show (FunctionVarNotInitialized ident) = "Function variable has to be initialized: " ++ show ident
  show (VoidInvalidType ident) = "Void type is not allowed for variable: " ++ show ident
  show (BuiltInFunctionOverride ident) = "Cannot override built-in function: " ++ show ident
  show (InvalidOperation type1 type2) = "Invalid operation between types: " ++ show type1 ++ " and " ++ show type2
  show MainFunctionMissing = "Main function missing"
