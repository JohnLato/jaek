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
import           Diagrams.Prelude hiding (apply)

import qualified Data.HashMap.Strict as M

import           Control.Parallel.Strategies
import           Control.Concurrent.STM

import           System.IO.Unsafe (unsafePerformIO)

getOffset :: View -> SampleCount
getOffset (WaveView off _) = off
getOffset _                = 0

getDur :: View -> SampleCount
getDur (WaveView _ dur) = dur
getDur _                = 44100    -- default duration, can do better

drawAt
  :: TVar PathMap
  -> FilePath
  -> TreeZip
  -> Maybe [Int]
  -> (Int,Int)
  -> ViewMap
  -> AnnDiagram Cairo R2 (First TreePath)
drawAt  mpRef  root zp Nothing    win   vmap =
  drawAt mpRef root zp (Just []) win vmap
drawAt _mpRef _root zp (Just []) (_x,_y) vmap =
  let tree    = fromZipper zp
      (FullView xScale yScale xOff yOff) = getView vmap tree
      -- using translations would be better than struts, but since
      -- the backend 'toGtkCoords' function auto-recenters, it's not possible
      -- at the moment.  I should fix that for diagrams-0.4
      xMod d = if xOff >= 0
                 then d ||| strutX xOff
                 else strutX (abs xOff) ||| d
      yMod d = if yOff >= 0
                 then strutY yOff === d
                 else d === strutY (abs yOff)
  in toGtkCoords
       . scaleY (180 * yScale)
       . scaleX (180 * xScale)
       . xMod
       . yMod
       $ drawTree tree
drawAt mpRef root zp (Just ref) (x,y) vmap =
  maybe (toGtkCoords . scale (0.25* fI x) . drawTree $ fromZipper zp)
        (\(z', v) -> let d = alignB
                             . vcat' (with {catMethod=Distrib, sep=1.001})
                             . map (centerY (strutY 1) |||)
                             . reverse
                             . parMap rseq (renderPeaks off dur x)
                             . unsafePerformIO
                             $ createReadPeaksForNode root mpRef z'
                         (_dx,dy) = size2D d
                         yscale = fI y / dy
                         off = fI $ getOffset v
                         dur = fI $ getDur v
                     in  d # scaleY yscale )
   $ (,) <$> goToRef (AbsPath ref) zp <*> M.lookup ref vmap

