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

type DrawRef m = TVar (QDiagram Cairo R2 m, View, Focus, (Int,Int))

initDrawRef :: (Monoid m, Semigroup m) => DrawingArea -> IO (DrawRef m)
initDrawRef da = do
  sz <- widgetGetSize da
  newTVarIO (mempty, FullView 0 0 0 0, Nothing, sz)

drawOnExpose ::
  (Monoid m, Semigroup m)
  => DrawingArea
  -> DrawRef m
  -> QDiagram Cairo R2 m
  -> ViewMap
  -> Focus
  -> (Diagram Cairo R2 -> Diagram Cairo R2)
  -> ()
  -> IO ()
drawOnExpose da ref d vue foc drawMod () = do
  dw <- widgetGetDrawWindow da
  curSz <- widgetGetSize da
  (lastD, lastView, lastFoc, lastSz) <- readTVarIO ref
  -- only sharing wavevues, because the full tree is quick to redraw
  -- and it can be updated without the vue/focus changing
  let thisView = foc >>= flip M.lookup vue
      chk    = (isWaveView <$> thisView) == Just True
  if foc == lastFoc && chk && thisView == Just lastView && curSz == lastSz
    then renderToGtk dw (drawMod (mempty <$ lastD))
    else do
      let newvue = fromMaybe (FullView 0 0 0 0) thisView
      atomically $ writeTVar ref (d, newvue, foc, curSz)
      renderToGtk dw (drawMod  (mempty <$ d))
