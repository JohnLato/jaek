{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Jaek.UI.Render (
  initDrawRef
 ,drawOnExpose
)

where

import Graphics.UI.Gtk
import Jaek.Base
import Jaek.UI.FrpHandlers
import Jaek.UI.FrpHandlersCustom
import Jaek.UI.Views

import Diagrams.Prelude
import Diagrams.Backend.Cairo.Gtk
import Diagrams.Backend.Cairo
import Data.Colour (withOpacity)

import qualified Data.HashMap.Strict as M

import Data.Maybe
import Control.Concurrent.STM

type DrawRef m = TVar (AnnDiagram Cairo R2 m, View, Focus, (Int,Int))

initDrawRef :: Monoid m => DrawingArea -> IO (DrawRef m)
initDrawRef da = do
  sz <- widgetGetSize da
  newTVarIO (mempty, (FullView 0 0), Nothing, sz)

drawOnExpose ::
  Monoid m
  => DrawingArea
  -> DrawRef m
  -> AnnDiagram Cairo R2 m
  -> ViewMap
  -> Focus
  -> [DragEvent]
  -> ()
  -> IO ()
drawOnExpose da ref d view focus sel () = do
  dw <- widgetGetDrawWindow da
  curSz <- widgetGetSize da
  (lastD, lastView, lastFoc, lastSz) <- readTVarIO ref
  -- only sharing waveviews, because the full tree is quick to redraw
  -- and it can be updated without the view/focus changing
  let thisView = focus >>= flip M.lookup view
      check    = (isWaveView <$> thisView) == Just True
  if focus == lastFoc && check && thisView == Just lastView && curSz == lastSz
    then renderToGtk dw (compositeSelection sel lastD)
    else do
      let newview = fromMaybe (FullView 0 0) thisView
      atomically $ writeTVar ref (d, newview, focus, curSz)
      renderToGtk dw (compositeSelection sel d)

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
drawSelection drags = foldr (\de d -> drawDrag de `atop` d) mempty drags

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
