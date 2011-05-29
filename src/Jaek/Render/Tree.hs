{-# LANGUAGE NoMonomorphismRestriction #-}

module Jaek.Render.Tree (
  drawTree
)

where

import Jaek.Tree
import Diagrams.Prelude
-- import Graphics.Rendering.Diagrams

import Data.Tree (Tree (..))

drawTree = drawTree'

-- adding the name because that way I can retrieve the associated NameMap.
-- At present, the 'circle' function creates an ellipse from the named points
-- C E N W S, which (presumably) are Center, East, North, West, South
-- I can use this to draw the lines, just go from Center to Center
-- for checking if a point is in the diagram, using the Query interface
-- seems more sensible, I just need to come up with the correct monoid,
-- one which indicates the node we're in.
drawTree' (Node dt childs) =
   (fmap (queryFunc $ nodePath dt) $ circle
        # scaleY 0.5
        # fc darkblue
        # lw 0.1
        # lc deepskyblue
        # pad 1.1
        # named (show $ nodePath dt)
   )
  ===
   (hcat' with {sep = 0.2} (map drawTree' childs) # centerX)

queryFunc :: TreePath -> Any -> First TreePath
queryFunc path any = if getAny any then First (Just path) else First Nothing
