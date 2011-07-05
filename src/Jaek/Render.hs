module Jaek.Render (
  drawAt
)

where

import           Jaek.Base
import           Jaek.Peaks
import           Jaek.Render.Stream
import           Jaek.Render.Tree
import           Jaek.Tree
import           Jaek.UI.Views

import           Diagrams.Backend.Cairo.Gtk
import           Diagrams.Prelude hiding (apply)

import qualified Data.HashMap.Strict as M

import           Control.Parallel.Strategies

import           System.IO.Unsafe (unsafePerformIO)

getOffset :: View -> SampleCount
getOffset (WaveView off _) = off
getOffset _                = 0

getDur :: View -> SampleCount
getDur (WaveView _ dur) = dur
getDur _                = 44100    -- default duration, can do better

-- for some reason the drawing isn't being shared properly.  I'm not sure if
-- it's Haskell not storing a function or Reactive.Banana.
drawAt _root zp Nothing    (x,_) _ =
  toGtkCoords . scale (0.25* fI x) . drawTree $ fromZipper zp
drawAt _root zp (Just [])  (x,_) _ =
  toGtkCoords . scale (0.25* fI x) . drawTree $ fromZipper zp
drawAt root zp (Just ref) (x,y) vmap =
  maybe (toGtkCoords . scale (0.25* fI x) . drawTree $ fromZipper zp)
        (\(z', v) -> let d = alignB . vcat
                             . reverse
                             . parMap rseq (renderPeaks off dur x)
                             . unsafePerformIO
                             $ createReadPeaksForNode root z'
                         (_dx,dy) = size2D d
                         yscale = fI y / dy
                         off = fI $ getOffset v
                         dur = fI $ getDur v
                     in  d # scaleY yscale )
   $ (,) <$> goToRef (AbsPath ref) zp <*> M.lookup ref vmap

