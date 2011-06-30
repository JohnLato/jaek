-- | Frp handlers specific to this program.  Will likely not be split
-- into a separate package.
module Jaek.UI.FrpHandlersCustom (
  Focus
 ,genBZip
 ,genBDraw
 ,genBFocus
 ,bCurSelection
 ,clickIsAdditive
)

where

import Jaek.Base
import Jaek.Render
import Jaek.StreamExpr
import Jaek.Tree
import Jaek.UI.Focus
import Jaek.UI.FrpHandlers
import Jaek.UI.Views

import Reactive.Banana  as FRP
import Diagrams.Prelude as D
import Diagrams.Backend.Cairo

import Data.Maybe

-- | generate the behavior of the zipper and the viewmap.  Since
-- the viewmap depends on the zipper, the two need to be created
-- together because there's no guaranteed ordering on behaviors.
genBZip ::
  HTree
  -> Event (String, HTree)
  -> Event (String, [StreamExpr])
  -> (Behavior TreeZip, Behavior ViewMap)
genBZip iTree eNewDoc eNewSource = (fst <$> bPair, snd <$> bPair)
 where
  bPair = accumB (zipper iTree, mapFromTree iTree) $
              ((\(_rt,ht) (_z,mp) ->
                  let nz = zipper ht
                  in (nz, updateMap nz NewDoc mp)) <$> eNewDoc)
           <> ((\(n1,n2) (zp,mp) ->
                 let z' = newSource n1 n2 zp
                 in (z', updateMap z' AddSrc mp)) <$> eNewSource)

-- | check if a drag event should be added to current selection (shift-drag)
-- or replace it.
dragIsAdditive :: DragEvent -> Bool
dragIsAdditive = clickIsAdditive . dragStart

clickIsAdditive :: ClickEvent -> Bool
clickIsAdditive = any (== ShiftE) . clickMods

bCurSelection :: Event DragEvent -> Event () -> Behavior [DragEvent]
bCurSelection eDrags eClear =
  accumB [] $ (dragAcc <$> eDrags) <> (clearAcc <$> eClear)
 where
  dragAcc drag acc
    | dragIsAdditive drag = drag:acc
    | otherwise           = [drag]
  clearAcc () _           = []

-- | Generate (Behavior (IO Focus), Event (IO Focus))
--  the @Event Focus@ are emitted when the focus changes, and can be used to
--  trigger screen refreshes
--  it's important to only trigger focus events when the focus actually changes
genBFocus :: Behavior (AnnDiagram Cairo R2 (First TreePath))
  -> Event ClickEvent
  -> (Behavior Focus, Event Focus )
genBFocus bDraw clicks = (beh, eFilt)
 where
  beh   = stepper Nothing eFilt
  eFilt  = filterApply ((/=) <$> beh) eFocus
  eFocus = filterE isJust $
            -- change from Tree to Wave
            FRP.apply ((\d clk -> getFirst $ runQuery (query d)
                                                      (P (xPos clk, yPos clk)) )
                                  <$> bDraw)
                      (filterApply ((const . isTree) <$> beh) clicks)
            -- change from Wave to Tree not implemented yet

-- | generate a Behavior Diagram producer
genBDraw ::
  Behavior FilePath
  -> Behavior TreeZip
  -> Behavior Focus
  -> Behavior (Int, Int)
  -> Behavior ViewMap
  -> Behavior (AnnDiagram Cairo R2 (First TreePath))
genBDraw bRoot bZip getFocus bsize bview =
  drawAt <$> bRoot <*> bZip <*> getFocus <*> bsize <*> bview


