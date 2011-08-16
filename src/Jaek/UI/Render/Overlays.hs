{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Jaek.UI.Render.Overlays (
  compositeSelection
)

where

import Jaek.Base
import Jaek.UI.FrpHandlers

import Diagrams.Prelude
import Data.Colour (withOpacity)

compositeSelection ::
  (Monoid m, Renderable (Path R2) b, Backend b R2)
  => [DragEvent]
  -> AnnDiagram b R2 m
  -> AnnDiagram b R2 m
compositeSelection drags d = drawSelection drags `atop` d

-- | overlay for selected regions
drawSelection ::
  (Monoid m, Renderable (Path R2) b, Backend b R2)
  => [DragEvent]
  -> AnnDiagram b R2 m
drawSelection = foldr (\de d -> drawDrag de `atop` d) mempty

drawDrag ::
  (Monoid m, Renderable (Path R2) b, Backend b R2)
  => DragEvent
  -> AnnDiagram b R2 m
drawDrag (DragE (ClickE _ _ sx sy) ex ey) = mempty <$>
  stroke (rect (abs $ ex - sx) (abs $ ey - sy))
   # alignBL
   # translate aVec
   # fcA (mediumpurple `withOpacity` 0.4)
 where
  aVec = (min sx ex, min sy ey)
