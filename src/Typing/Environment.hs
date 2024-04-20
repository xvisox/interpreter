module Typing.Environment where

import Data.Map
import AbsSeeemcrd(Ident)

import Typing.Types

data Env = Env {
  variables :: Map Ident TCType,
  hasReturn :: Bool,
  returnType :: TCType
}