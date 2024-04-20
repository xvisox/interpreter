module Common where

import AbsSeeemcrd(BNFC'Position)

type Pos = BNFC'Position
showPos :: Pos -> String
showPos Nothing = "No position"
showPos (Just (line, column)) = "Line " ++ show line ++ ", Column " ++ show column
