module Interpreter.Environment where

import Data.Map
import AbsSeeemcrd(Ident)

data EVal = EInt Int
          | EBool Bool
          | EString String

instance Show EVal where
  show (EInt i) = show i
  show (EBool b) = show b
  show (EString s) = s

type Loc = Int

type Store = Map Loc EVal

initStore :: Store
initStore = Data.Map.empty

data Env = Env {
  variables :: Map Ident Loc
}

initEnv :: Env
initEnv = Env {
  variables = Data.Map.empty
}
