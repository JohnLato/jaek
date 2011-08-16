-- | Frp handlers specific to this program.  Will likely not be split
-- into a separate package.
module Jaek.UI.FrpHandlersCustom (
  Focus
 ,genBZip
 ,genBDraw
 ,genBFocus
)

where

import Jaek.Base
import Jaek.Render
import Jaek.Peaks
import Jaek.StreamExpr
import Jaek.Tree
import Jaek.UI.Focus
import Jaek.UI.FrpHandlers
import Jaek.UI.Views

import Reactive.Banana  as FRP
import Diagrams.Prelude as D
import Diagrams.Backend.Cairo
import Data.Label as L

import Data.Maybe
import Control.Concurrent.STM

-- | generate the behavior of the zipper and the viewmap.  Since
-- the viewmap depends on the zipper, the two need to be created
-- together because there's no guaranteed ordering on behaviors.
genBZip ::
  HTree
  -> Event (String, HTree)
  -> Event (String, [StreamExpr])
  -> Event (TreeZip -> TreeZip)
  -> Event Focus
  -> (Behavior TreeZip, Behavior ViewMap)
genBZip iTree eNewDoc eNewSource eTreeMod eFocChange =
  (fst <$> bPair, snd <$> bPair)
 where
  bPair = accumB (zipper iTree, mapFromTree iTree) $
              ((\(_rt,ht) (_z,mp) ->
                  let z' = zipper ht
                  in (z', updateMap z' NewDoc mp)) <$> eNewDoc)
           <> ((\(n1,n2) (zp,mp) ->
                  let z' = newSource n1 n2 zp
                  in (z', updateMap z' AddSrc mp)) <$> eNewSource)
           <> ((\updateF (zp,mp) ->
                  let z' = updateF zp
                  in (z', updateMap z' MdNode mp)) <$> eTreeMod)
           <> ((\newFoc (zp,mp) -> (goToFocus zp newFoc, mp)) <$> eFocChange)

-- | Generate @Discrete (IO Focus)@
--  the @Event Focus@ are emitted when the focus changes, and can be used to
--  trigger screen refreshes
--  it's important to only trigger focus events when the focus actually changes
genBFocus :: Behavior (AnnDiagram Cairo R2 (First TreePath))
  -> Event ClickEvent
  -> Event Focus
  -> Event TreeZip
  -> Discrete Focus
genBFocus bDraw clicks eFocChange eTreeChange = dfoc
 where
  dfoc   = stepperD Nothing eFilt
  beh    = value dfoc
  eFilt  = filterApply ((/=) <$> beh) eFocus
  eFocus = (Just . getPath <$> eTreeChange)
           <> filterE isJust (
            -- change from Tree to Wave
            FRP.apply ((\d clk -> getFirst $ runQuery (query d)
                                                      (P $ L.get xyClick clk) )
                                  <$> bDraw)
                      (filterApply ((const . isTree) <$> beh) clicks) )
            -- change from Wave to Tree
           <> eFocChange

-- | generate a Behavior Diagram producer
genBDraw ::
  TVar PathMap
  -> Behavior FilePath
  -> Behavior TreeZip
  -> Behavior Focus
  -> Behavior (Int, Int)
  -> Behavior ViewMap
  -> Behavior (AnnDiagram Cairo R2 (First TreePath))
genBDraw mpRef bRoot bZip getFocus bsize bview =
  drawAt mpRef <$> bRoot <*> bZip <*> getFocus <*> bsize <*> bview
