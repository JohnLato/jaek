{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Jaek.UI.Render (
  initDrawRef
 ,drawOnExpose
)

where

import Graphics.UI.Gtk
import Jaek.Base
import Jaek.UI.FrpHandlersCustom
import Jaek.UI.Views

import Diagrams.Prelude
import Diagrams.Backend.Cairo.Gtk
import Diagrams.Backend.Cairo

import qualified Data.HashMap.Strict as M

import Data.Maybe
import Control.Concurrent.STM

type DrawRef m = TVar (AnnDiagram Cairo R2 m, View, Focus, (Int,Int))

initDrawRef :: Monoid m => DrawingArea -> IO (DrawRef m)
initDrawRef da = do
  sz <- widgetGetSize da
  newTVarIO (mempty, FullView 0 0 0 0, Nothing, sz)

drawOnExpose ::
  Monoid m
  => DrawingArea
  -> DrawRef m
  -> AnnDiagram Cairo R2 m
  -> ViewMap
  -> Focus
  -> (Diagram Cairo R2 -> Diagram Cairo R2)
  -> ()
  -> IO ()
drawOnExpose da ref d view foc drawMod () = do
  dw <- widgetGetDrawWindow da
  curSz <- widgetGetSize da
  (lastD, lastView, lastFoc, lastSz) <- readTVarIO ref
  -- only sharing waveviews, because the full tree is quick to redraw
  -- and it can be updated without the view/focus changing
  let thisView = foc >>= flip M.lookup view
      chk    = (isWaveView <$> thisView) == Just True
  if foc == lastFoc && chk && thisView == Just lastView && curSz == lastSz
    then renderToGtk dw (drawMod (mempty <$ lastD))
    else do
      let newview = fromMaybe (FullView 0 0 0 0) thisView
      atomically $ writeTVar ref (d, newview, foc, curSz)
      renderToGtk dw (drawMod  (mempty <$ d))
