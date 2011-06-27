-- | Frp handlers specific to this program.  Will likely not be split
-- into a separate package.
module Jaek.UI.FrpHandlersCustom (
  genBZip
 ,bCurSelection
 ,clickIsAdditive
)

where

import Jaek.Base
import Jaek.StreamExpr
import Jaek.Tree
import Jaek.UI.Views
import Jaek.UI.FrpHandlers

import Reactive.Banana
import Diagrams.Prelude ((<>))

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
