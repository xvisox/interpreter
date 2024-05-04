module Typing.Types where

import AbsSeeemcrd

data TCArgKind
  = TCArgRef
  | TCArgVal
  deriving (Eq, Read)

instance Show TCArgKind where
  show TCArgRef = "&"
  show TCArgVal = ""

data TCType
  = TCFun [TCArgType] TCType
  | TCInt
  | TCBool
  | TCString
  | TCVoid
  deriving (Eq, Read)

instance Show TCType where
  show (TCFun args returnType) = "(" ++ (Prelude.concat $ Prelude.map show args) ++ ") -> " ++ show returnType
  show TCInt = "int"
  show TCBool = "bool"
  show TCString = "string"
  show TCVoid = "void"

data TCArgType = TCArgType TCArgKind TCType
  deriving (Eq, Read)

instance Show TCArgType where
  show (TCArgType argKind argType) = show argKind ++ show argType

extractType :: TCArgType -> TCType
extractType (TCArgType _ argType) = argType

mapToTCType :: Type -> TCType
mapToTCType (Int _) = TCInt
mapToTCType (Str _) = TCString
mapToTCType (Bool _) = TCBool
mapToTCType (Void _) = TCVoid
mapToTCType (Fun _ returnType args) = TCFun (Prelude.map mapToTCArgType args) (mapToTCType returnType)

mapToTCArgType :: ArgType -> TCArgType
mapToTCArgType (ValArg _ argType) = TCArgType TCArgVal (mapToTCType argType)
mapToTCArgType (RefArg _ argType) = TCArgType TCArgRef (mapToTCType argType)

mapToTCArgTypes :: [Arg] -> [TCArgType]
mapToTCArgTypes = Prelude.map (\(AArg _ argType _) -> mapToTCArgType argType)

mapToTypesWithIdents :: [Arg] -> [(TCType, Ident)]
mapToTypesWithIdents = Prelude.map (\(AArg _ argType ident) -> (extractType $ mapToTCArgType argType, ident))