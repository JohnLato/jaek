module Jaek.UI.Controllers.Edit1 (
  keyActions
)

where

import Graphics.UI.Gtk
import Jaek.SegmentOlaps
import Jaek.Tree
import Jaek.UI.FrpHandlers
import Jaek.UI.Controllers.Base
import Jaek.UI.Controllers.Drags
import Jaek.UI.Views

import Reactive.Banana

import Data.Function as F
import Data.List
import Data.Ord
import Data.Maybe (isJust)
import Data.Monoid (Monoid (..))

keyActions :: 
  Discrete (Int,Int)
  -> Discrete ViewMap
  -> Controller [DragEvent]
  -> Event KeyVal
  -- -> Event (TreeZip -> TreeZip)
  -> Controller ()
keyActions bSz bVm selCtrl eKey =
  nullController { dActive = ((\sAct sel -> sAct && not (null sel))
                     <$> dActive selCtrl <*> dState selCtrl)
                     -- active with current selection
                  ,dState = pure ()
                  ,keysPred = keyPred
                  ,eZipChange = zChng }
 where
  zChng = filterMaybes $
            (keyactOnSelect <$> bSz <*> bVm <*> dState selCtrl) <@> eKey
  keyPred _ k = isJust $ keyactOnSelect undefined mempty [undefined] k

keyactOnSelect ::
  (Int,Int)
  -> ViewMap
  -> [DragEvent]
  -> KeyVal
  -> Maybe (TreeZip -> TreeZip)
keyactOnSelect sz vm sels key
  | sels == []                = Nothing
  | keyToChar key == Just 'm' = Just $ \tz -> mkMute (mkRegions tz sels) tz
  | keyToChar key == Just 'd' = Just $ \tz -> mkCut (mkRegions tz sels) tz
  | otherwise                 = Nothing
  
 where
  unT :: [(a,b)] -> (a,[b])
  unT xs = let ix = fst $ head xs
           in (ix, map snd xs)
  -- reT :: Num b => [(a, [(b,b)])] -> [(a,b,b)]
  -- reT converts the channelized data back to unchannelized tuples,
  -- and also converts from the start/end representation to offset/dur
  -- representation
  reT = concatMap (\(a, ys) -> map (\(b,c) -> (a,b,c-b)) ys)

  -- this slightly convoluted function removes overlapping segments
  -- in each channel.
  mkRegions tz = reT
                 . (fmap . fmap) removeOlaps
                 . map unT                      -- [(chn, [(off,dur)])]
                 . groupBy ((==) `F.on` fst)
                 . sortBy (comparing fst)
                 . concatMap (dragToRegions sz tz vm)
