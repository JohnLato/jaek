module Jaek.Render (
  bDrawF
 ,focusF
)

where

import Jaek.Base
import Jaek.Render.Stream
import Jaek.Render.Tree
import Jaek.Tree
import Jaek.UI.MenuActionHandlers

import Reactive.Banana as FRP
import Diagrams.Backend.Cairo.Gtk
import Diagrams.Prelude hiding (apply)

import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

drawAt zp Nothing    = toGtkCoords . scale 150 . drawTree $ fromZipper zp
drawAt zp (Just [])  = toGtkCoords . scale 150 . drawTree $ fromZipper zp
drawAt zp (Just ref) =
  maybe (toGtkCoords . scale 150 . drawTree $ fromZipper zp)
        (scaleX 0.3 . scaleY 200 . alignB . vcat
         . map (unsafePerformIO . exprPeaks) . getExprs . hole)
   $ goToRef (AbsPath ref) zp

-- | generate a Behavior Diagram
bDrawF bZip bFocus = drawAt <$> bZip <*> bFocus

-- | Generate (Behavior Focus, Event Focus)
--  the @Event Focus@ are emitted when the focus changes, and can be used to
--  trigger screen refreshes
focusF bDraw clicks = (accumB Nothing (const <$> eFocus), eFocus)
 where
  eFocus = FRP.filter isJust $
             apply ((\d clk -> getFirst $ runQuery (query d)
                                                   (P (xPos clk, yPos clk)) )
                               <$> bDraw)
                    clicks
