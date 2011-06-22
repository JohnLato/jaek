module Jaek.Render (
  genBDraw
 ,genBFocus
)

where

import           Jaek.Base
import           Jaek.Peaks
import           Jaek.Render.Stream
import           Jaek.Render.Tree
import           Jaek.Tree
import           Jaek.UI.MenuActionHandlers
import           Jaek.UI.Views

import           Reactive.Banana as FRP
import           Diagrams.Backend.Cairo.Gtk
import           Diagrams.Prelude hiding (apply)
import           Diagrams.Backend.Cairo

import qualified Data.HashMap.Strict as M

import           Control.Parallel.Strategies

import           Data.Maybe
import           System.IO.Unsafe (unsafePerformIO)

getOffset :: View -> SampleCount
getOffset (WaveView off _) = off
getOffset _                = 0

getDur :: View -> SampleCount
getDur (WaveView _ dur) = dur
getDur _                = 44100    -- default duration, can do better

drawAt _root zp Nothing    (x,_) _ =
  toGtkCoords . scale (0.25* fI x) . drawTree $ fromZipper zp
drawAt _root zp (Just [])  (x,_) _ =
  toGtkCoords . scale (0.25* fI x) . drawTree $ fromZipper zp
drawAt root zp (Just ref) (x,y) vmap =
  maybe (toGtkCoords . scale (0.25* fI x) . drawTree $ fromZipper zp)
        (\(z', v) -> let d = alignB . vcat
                             . parMap rseq (renderPeaks off dur x)
                             . unsafePerformIO
                             $ createReadPeaksForNode root z'
                         (_dx,dy) = size2D d
                         yscale = fI y / dy
                         off = fI $ getOffset v
                         dur = fI $ getDur v
                     in  d # scaleY yscale )
   $ (,) <$> goToRef (AbsPath ref) zp <*> M.lookup ref vmap

-- | generate a Behavior Diagram producer
genBDraw ::
  Behavior FilePath
  -> Behavior TreeZip
  -> Behavior (Maybe [Int])
  -> Behavior (Int, Int)
  -> Behavior ViewMap
  -> Behavior (AnnDiagram Cairo R2 (First TreePath))
genBDraw bRoot bZip getFocus bsize bview =
  drawAt <$> bRoot <*> bZip <*> getFocus <*> bsize <*> bview

-- | Generate (Behavior (IO Focus), Event (IO Focus))
--  the @Event Focus@ are emitted when the focus changes, and can be used to
--  trigger screen refreshes
genBFocus :: Behavior (AnnDiagram Cairo R2 (First TreePath))
  -> Event ClickEvent
  -> (Behavior (Maybe [Int]), Event (Maybe [Int]) )
genBFocus bDraw clicks = (accumB Nothing (const <$> eFocus), eFocus)
 where
  eFocus = FRP.filter (isJust) $
            apply ((\d clk -> getFirst $ runQuery (query d)
                                                  (P (xPos clk, yPos clk)) )
                              <$> bDraw)
                   clicks
