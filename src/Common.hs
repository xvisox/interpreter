module Common where

import AbsSeeemcrd(BNFC'Position, Ident(..))
import Typing.Types

type Pos = BNFC'Position
showPos :: Pos -> String
showPos Nothing = "no position"
showPos (Just (line, column)) = "line " ++ show line ++ ", column " ++ show column

builtInFunctions :: [(Ident, (TCType, [TCArgType]))]
builtInFunctions = [
    (Ident "printStr", (TCVoid, [TCArgType TCVal TCString])),
    (Ident "printInt", (TCVoid, [TCArgType TCVal TCInt])),
    (Ident "printBool", (TCVoid, [TCArgType TCVal TCBool])),
    (Ident "toStr", (TCString, [TCArgType TCVal TCInt])),
    (Ident "toInt", (TCInt, [TCArgType TCVal TCString])),
    (Ident "exit", (TCVoid, []))
  ]
