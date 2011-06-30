module Jaek.UI.Focus (
  Focus
 ,isTree
 ,isWave
)

where

type Focus = Maybe [Int]

isTree :: Focus -> Bool
isTree Nothing   = True
isTree (Just []) = True
isTree _         = False

isWave :: Focus -> Bool
isWave = not . isTree
