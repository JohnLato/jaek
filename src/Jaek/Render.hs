module Jaek.Render (
  View (..)
 ,genBDraw
 ,genBFocus
)

where

import Jaek.Base
import Jaek.Render.Stream
import Jaek.Render.Tree
import Jaek.Tree
import Jaek.UI.MenuActionHandlers

import Graphics.UI.Gtk (DrawingArea)

import Reactive.Banana as FRP
import Diagrams.Backend.Cairo.Gtk
import Diagrams.Prelude hiding (apply)
import Diagrams.Backend.Cairo

import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

-- | Information about what's currently in view...
data View =
   FullView !Double !Double           -- ^ xScale, yScale
 | WaveView !SampleCount !SampleCount -- ^ streamOff, streamDur
 deriving (Eq, Show)

getOffset :: View -> SampleCount
getOffset (WaveView off _) = off
getOffset _                = 0

getDur :: View -> SampleCount
getDur (WaveView _ dur) = dur
getDur _                = 44100    -- default duration, can do better

drawAt zp Nothing    (x,_) _ = toGtkCoords . scale (0.25* fI x) . drawTree $
  fromZipper zp
drawAt zp (Just [])  (x,_) _ = toGtkCoords . scale (0.25* fI x) . drawTree $
  fromZipper zp
drawAt zp (Just ref) (x,y) view =
  maybe (toGtkCoords . scale (0.25* fI x) . drawTree $ fromZipper zp)
        (\z' -> let d = alignB . vcat
                        . map (unsafePerformIO . exprPeaks off dur x)
                        . getExprs $ hole z'
                    (_dx,dy) = size2D d
                    yscale = fI y / dy
                in  d # scaleY yscale )
   $ goToRef (AbsPath ref) zp
 where
  off = fI $ getOffset view
  dur = fI $ getDur view

-- | generate a Behavior Diagram producer
genBDraw ::
  Behavior TreeZip
  -> Behavior (Maybe [Int])
  -> Behavior (Int, Int)
  -> Behavior View
  -> Behavior (AnnDiagram Cairo R2 (First TreePath))
genBDraw bZip getFocus bsize bview =
  drawAt <$> bZip <*> getFocus <*> bsize <*> bview

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
