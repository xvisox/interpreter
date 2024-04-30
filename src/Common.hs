module Common where

import AbsSeeemcrd(BNFC'Position, Ident(..))
import Typing.Types

type Pos = BNFC'Position
showPos :: Pos -> String
showPos Nothing = "no position"
showPos (Just (line, column)) = "line " ++ show line ++ ", column " ++ show column

builtInFunctions :: [(Ident, (TCType, [TCArgType]))]
builtInFunctions = [
    (Ident "printStr", (TCVoid, [TCArgType TCArgVal TCString])),
    (Ident "printInt", (TCVoid, [TCArgType TCArgVal TCInt])),
    (Ident "printBool", (TCVoid, [TCArgType TCArgVal TCBool])),
    (Ident "toStr", (TCString, [TCArgType TCArgVal TCInt])),
    (Ident "toInt", (TCInt, [TCArgType TCArgVal TCString])),
    (Ident "exit", (TCVoid, []))
  ]
