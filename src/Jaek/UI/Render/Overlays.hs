{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Jaek.UI.Render.Overlays (
  compositeSelection
)

where

import Jaek.Base
import Jaek.UI.FrpHandlers

import Diagrams.Prelude
import Data.Colour (Colour, withOpacity)

compositeSelection ::
  (Monoid m, Semigroup m, Renderable (Path R2) b, Backend b R2)
  => [DragEvent]          -- the selection
  -> [DragEvent]          -- the current drag region
  -> QDiagram b R2 m
  -> QDiagram b R2 m
compositeSelection drags curs d =
  drawSelection mediumpurple curs
  `atop` drawSelection royalblue drags
  `atop` d

-- | overlay for selected regions
drawSelection ::
  (Monoid m, Semigroup m, Renderable (Path R2) b, Backend b R2)
  => Colour Double
  -> [DragEvent]
  -> QDiagram b R2 m
drawSelection colr = foldr (\de d -> drawDrag colr de `atop` d) mempty

drawDrag ::
  (Monoid m, Semigroup m, Renderable (Path R2) b, Backend b R2)
  => Colour Double
  -> DragEvent
  -> QDiagram b R2 m
drawDrag colr (DragE (ClickE _ _ sx sy) ex ey) = mempty <$>
  stroke (rect (abs $ ex - sx) (abs $ ey - sy))
   # alignBL
   # translate aVec
   # fcA (colr `withOpacity` 0.4)
 where
  aVec = r2 (min sx ex, min sy ey)
