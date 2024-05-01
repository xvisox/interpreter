module Interpreter.Environment where

import Data.Map
import Data.Maybe(fromJust)
import AbsSeeemcrd

data IArgKind = IArgRef | IArgVal

type IArg = (IArgKind, Ident)

data IVal = IInt Int
          | IBool Bool
          | IString String
          | IFunc [IArg] Block Env
          | IVoid

instance Show IVal where
  show (IInt i) = show i
  show (IBool b) = show b
  show (IString s) = s
  show (IFunc _ _ _) = "function"
  show IVoid = "void"

mapToDefaultIVal :: Type -> IVal
mapToDefaultIVal (Int _) = IInt 0
mapToDefaultIVal (Bool _) = IBool False
mapToDefaultIVal (Str _) = IString ""
mapToDefaultIVal (Void _) = IVoid
mapToDefaultIVal _ = error "Invalid type"

type Loc = Int

newLoc :: Store -> Loc
newLoc store = Data.Map.size store

insertLoc :: Loc -> IVal -> Store -> Store
insertLoc loc val store = Data.Map.insert loc val store

lookupLoc :: Loc -> Store -> IVal
lookupLoc loc store = fromJust $ Data.Map.lookup loc store

type Store = Map Loc IVal

initStore :: Store
initStore = Data.Map.empty

data Env = Env {
  variables :: Map Ident Loc
}

initEnv :: Env
initEnv = Env {
  variables = Data.Map.empty
}

insertNewVar :: Ident -> Loc -> Env -> Env
insertNewVar ident loc env = env { variables = insert ident loc (variables env) }

lookupVar :: Ident -> Env -> Loc
lookupVar ident env = fromJust $ Data.Map.lookup ident (variables env)
