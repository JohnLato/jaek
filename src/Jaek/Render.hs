module Jaek.Render (
  genBDraw
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

drawAt zp Nothing    (x,_) = toGtkCoords . scale (0.25*x) . drawTree $
  fromZipper zp
drawAt zp (Just [])  (x,_) = toGtkCoords . scale (0.25*x) . drawTree $
  fromZipper zp
drawAt zp (Just ref) (x,y) =
  maybe (toGtkCoords . scale (0.25*x) . drawTree $ fromZipper zp)
        (\z' -> let d = alignB . vcat . map (unsafePerformIO . exprPeaks) .
                          getExprs $ hole z'
                    (dx,dy) = size2D d
                    xscale = x / dx
                    yscale = y / dy
                in  d # scaleX xscale # scaleY yscale )
   $ goToRef (AbsPath ref) zp

-- | generate a Behavior Diagram
genBDraw ::
  Behavior TreeZip
  -> Behavior (Maybe [Int])
  -> Behavior (Double, Double)
  -> Behavior (AnnDiagram Cairo R2 (First TreePath))
genBDraw bZip getFocus bsize = drawAt <$> bZip <*> getFocus <*> bsize

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
