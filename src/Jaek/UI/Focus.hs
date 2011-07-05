module Jaek.UI.Focus (
  Focus
 ,isTree
 ,isWave
 ,goToFocus
)

where

import Jaek.Tree
import Data.Maybe

type Focus = Maybe [Int]

isTree :: Focus -> Bool
isTree Nothing   = True
isTree (Just []) = True
isTree _         = False

isWave :: Focus -> Bool
isWave = not . isTree

goToFocus :: TreeZip -> Focus -> TreeZip
goToFocus tz foc = fromMaybe tz $ foc >>= \fc -> goToRef (AbsPath fc) tz
