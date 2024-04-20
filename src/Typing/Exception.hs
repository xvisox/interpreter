module Typing.Exception where

import Prelude
import AbsSeeemcrd(Ident)

import Common
import Typing.Types

data TypeCheckError = TypeCheckError Pos TypeCheckException

instance Show TypeCheckError where
  show (TypeCheckError pos ex) = show ex ++ " at " ++ showPos pos

data TypeCheckException
  = AssignmentMismatch TCType TCType
  | Redeclared Ident
  | NoVariable Ident
  | NotAFunction Ident
  | WrongArgumentCount Int Int
  | ArgumentMismatch TCArgType TCArgType

instance Show TypeCheckException where
  show (AssignmentMismatch expected actual) = "Assignment mismatch: expected " ++ show expected ++ ", got " ++ show actual
  show (Redeclared ident) = "Redeclared variable: " ++ show ident
  show (NoVariable ident) = "No such variable: " ++ show ident
  show (NotAFunction ident) = "Not a function: " ++ show ident
  show (WrongArgumentCount expected actual) = "Wrong argument count: expected " ++ show expected ++ ", got " ++ show actual
  show (ArgumentMismatch expected actual) = "Argument mismatch: expected " ++ show expected ++ ", got " ++ show actual
