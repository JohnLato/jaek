-- | Frp handlers specific to this program.  Will likely not be split
-- into a separate package.
module Jaek.UI.FrpHandlersCustom (
  genBZip
)

where

import Jaek.Base
import Jaek.StreamExpr
import Jaek.Tree
import Jaek.UI.Views

import Reactive.Banana
import Diagrams.Prelude ((<>))

-- | generate the behavior of the zipper and the viewmap.  Since
-- the viewmap depends on the zipper, the two need to be created
-- together because there's no guaranteed ordering on behaviors.
genBZip ::
  Event String
  -> Event (String, [StreamExpr])
  -> (Behavior TreeZip, Behavior ViewMap)
genBZip eNewDoc eNewSource = (fst <$> bPair, snd <$> bPair)
 where
  bPair = accumB (iZip, iMap) $
              ((\(_z,mp) -> (iZip, updateMap iZip NewDoc mp)) <$ eNewDoc)
           <> ((\(n1,n2) (zp,mp) ->
                 let z' = newSource n1 n2 zp
                 in (z', updateMap z' AddSrc mp)) <$> eNewSource)
