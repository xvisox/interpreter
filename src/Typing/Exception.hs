module Typing.Exception where

import Prelude
import AbsSeeemcrd(Ident)

import Common
import Typing.Types

data TypeCheckException
  = AssignmentMismatch TCType TCType Pos
  | Redeclared Ident
  | NoVariable Ident

instance Show TypeCheckException where
  show (AssignmentMismatch expected actual pos) = "Assignment mismatch: expected " ++ show expected ++ ", got " ++ show actual ++ " at " ++ showPos pos
  show (Redeclared ident) = "Redeclared variable: " ++ show ident
