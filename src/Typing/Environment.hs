module Typing.Environment where

import Data.Map
import AbsSeeemcrd(Ident)

import Typing.Types

type Scope = Integer

data Env = Env {
  scope :: Scope,
  variables :: Map Ident (TCType, Scope),
  hasReturn :: Bool,
  returnType :: TCType
}

initEnv :: Env
initEnv = Env {
  scope = 0,
  variables = Data.Map.empty,
  hasReturn = False,
  returnType = TCVoid
}

lookupVar :: Ident -> Env -> Maybe (TCType, Scope)
lookupVar ident env = Data.Map.lookup ident (variables env)

insertVar :: Ident -> TCType -> Env -> Env
insertVar ident varType env = env { variables = Data.Map.insert ident (varType, scope env) (variables env) }

newScope :: Env -> Env
newScope env = env { scope = scope env + 1 }
