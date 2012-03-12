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

import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Gtk
import           Diagrams.Prelude hiding (apply, First, Duration)

import qualified Data.HashMap.Strict as M

import           Data.Monoid
import           Control.Concurrent.STM
import           Control.Parallel.Strategies

import           System.IO.Unsafe (unsafePerformIO)

getOffset :: View -> SampleCount
getOffset (WaveView off _) = off
getOffset _                = 0

getDur :: View -> Duration
getDur (WaveView _ dur) = dur
getDur _                = 44100    -- default duration, can do better

drawAt
  :: TVar PathMap
  -> FilePath
  -> TreeZip
  -> Maybe [Int]
  -> (Int,Int)
  -> ViewMap
  -> QDiagram Cairo R2 (First TreePath)
drawAt  mpRef  root zp Nothing    win   vmap =
  drawAt mpRef root zp (Just []) win vmap
drawAt _mpRef _root zp (Just []) _win vmap =
  let tree    = fromZipper zp
      (FullView xDist yDist xOff yOff) = getView vmap tree
      xScale = recip xDist
      yScale = recip yDist
      szCon  = 180
      -- since the backend 'toGtkCoords' function auto-recenters,
      -- it's not possible to use it
      -- at the moment.  I should fix that for diagrams-0.6
      pos d = let p = unp2 $ center2D d
              in translate (r2 $ szCon *^ (negate xOff, yOff) ^-^ p) d
  in   pos
       . reflectY
       . scaleY (szCon * yScale)
       . scaleX (szCon * xScale)
       $ drawTree tree
drawAt mpRef root zp (Just ref) (x,y) vmap =
  maybe (toGtkCoords . scale (0.25* fI x) . drawTree $ fromZipper zp)
        (\(z', v) -> let d = alignB
                             . vcat' (with {catMethod=Distrib, sep=2.001})
                             . map (centerY (strutY 2) |||)
                             . reverse
                             . parMap rseq (renderPeaks x)
                             . unsafePerformIO
                             $ createReadPeaksForNode root off dur x mpRef z'
                         (_dx,dy) = size2D d
                         yscale = fI y / dy
                         off = fI $ getOffset v
                         dur = fI $ getDur v
                     in  d # scaleY yscale )
   $ (,) <$> goToRef (AbsPath ref) zp <*> M.lookup ref vmap

