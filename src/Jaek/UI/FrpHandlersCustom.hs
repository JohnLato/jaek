-- | Frp handlers specific to this program.  Will likely not be split
-- into a separate package.
module Jaek.UI.FrpHandlersCustom (
  Focus
 ,genBZip
 ,genBDraw
 ,genDFocus
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
import Diagrams.Prelude as D hiding (First (..))
import Diagrams.Backend.Cairo
import Data.Label as L

import Data.Maybe
import Data.Monoid
import Control.Arrow (second)
import Control.Concurrent.STM

-- | generate the behavior of the zipper and the viewmap.  Since
-- the viewmap depends on the zipper, the two need to be created
-- together because there's no guaranteed ordering on behaviors.
genBZip ::
  HTree
  -> Event (String, HTree)
  -> Event (String, [StreamExpr])
  -> Event (TreeZip -> TreeZip)
  -> Event (ViewMap -> ViewMap)
  -> Event Focus
  -> (Discrete TreeZip, Discrete ViewMap)
genBZip iTree eNewDoc eNewSource eTreeMod eViewMod eFocChange =
  (fst <$> bPair, snd <$> bPair)
 where
  bPair = accumD (zipper iTree, mapFromTree iTree) $ mconcat
          [(\(_rt,ht) (_z,mp) ->
                  let z' = zipper ht
                  in (z', updateMap z' NewDoc mp)) <$> eNewDoc
          ,(\(n1,n2) (zp,mp) ->
                  let z' = newSource n1 n2 zp
                  in (z', updateMap z' AddSrc mp)) <$> eNewSource
          ,(\updateF (zp,mp) ->
                  let z' = updateF zp
                  in (z', updateMap z' MdNode mp)) <$> eTreeMod
          ,(\newFoc (zp,mp) -> (goToFocus zp newFoc, mp)) <$> eFocChange
          ,second <$> eViewMod ]

-- | Generate @Discrete (IO Focus)@
--  the @Event Focus@ are emitted when the focus changes, and can be used to
--  trigger screen refreshes
--  it's important to only trigger focus events when the focus actually changes
genDFocus :: Behavior (QDiagram Cairo R2 (First TreePath))
  -> Event ClickEvent
  -> Event Focus
  -> Event TreeZip
  -> Discrete Focus
genDFocus bDraw clicks eFocChange eTreeChange = dfoc
 where
  dfoc   = stepperD Nothing eFilt
  beh    = FRP.value dfoc
  eFilt  = filterApply ((/=) <$> beh) eFocus
  eFocus = (Just . getPath <$> eTreeChange)
           `mappend` filterE isJust (
            -- change from Tree to Wave
            FRP.apply ((\d clk -> getFirst $ runQuery (query d)
                                                      (p2 $ L.get xyClick clk))
                                  <$> bDraw)
                      (filterApply ((const . isTree) <$> beh) clicks) )
            -- change from Wave to Tree
           `mappend` eFocChange

-- | generate a Behavior Diagram producer
genBDraw ::
  TVar PathMap
  -> Behavior FilePath
  -> Behavior TreeZip
  -> Behavior Focus
  -> Behavior (Int, Int)
  -> Behavior ViewMap
  -> Behavior (QDiagram Cairo R2 (First TreePath))
genBDraw mpRef bRoot bZip getFocus bsize bview =
  drawAt mpRef <$> bRoot <*> bZip <*> getFocus <*> bsize <*> bview
