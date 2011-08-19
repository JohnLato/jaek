module Jaek.UI.Focus (
  Focus
 ,isTree
 ,isWave
 ,goToFocus
 ,treeFocus
)

where

import Jaek.Tree
import Data.Maybe

type Focus = Maybe [Int]

treeFocus :: Focus
treeFocus = Just []

isTree :: Focus -> Bool
isTree Nothing   = True
isTree (Just []) = True
isTree _         = False

isWave :: Focus -> Bool
isWave = not . isTree

goToFocus :: TreeZip -> Focus -> TreeZip
goToFocus tz foc = fromMaybe (goToHead tz)
                     $ foc >>= \fc -> goToRef (AbsPath fc) tz
