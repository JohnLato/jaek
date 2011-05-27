{-# LANGUAGE NoMonomorphismRestriction #-}

module Jaek.Render.Tree (
  drawTree
)

where

import Diagrams.Prelude

import Data.Tree (Tree (..))

drawTree (Node dt childs) =
  circle # scaleY 0.5 # fc darkblue # lw 0.1 # lc deepskyblue # pad 1.1
 ===
  (hcat' with {sep = 0.2} (map drawTree childs) # centerX)
