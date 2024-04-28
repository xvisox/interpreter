module Interpreter.Environment where

import Data.Map
import AbsSeeemcrd

type IArg = Ident

data IVal = IInt Int
          | IBool Bool
          | IString String
          | IRef Loc
          | IVoid
          | IFunc [IArg] Block Env

instance Show IVal where
  show (IInt i) = show i
  show (IBool b) = show b
  show (IString s) = s
  show (IRef l) = show l
  show IVoid = "void"

mapToDefaultIVal :: Type -> IVal
mapToDefaultIVal (Int _) = IInt 0
mapToDefaultIVal (Bool _) = IBool False
mapToDefaultIVal (Str _) = IString ""

type Loc = Int

newLoc :: Store -> Loc
newLoc store = Data.Map.size store

insertLoc :: Loc -> IVal -> Store -> Store
insertLoc loc val store = Data.Map.insert loc val store

lookupLoc :: Loc -> Store -> Maybe IVal
lookupLoc loc store = Data.Map.lookup loc store

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

lookupVar :: Ident -> Env -> Maybe Loc
lookupVar ident env = Data.Map.lookup ident (variables env)
