module Jaek.UI.Input.Actions (
  keyActions
)

where

import Graphics.UI.Gtk
import Jaek.SegmentOlaps
import Jaek.Tree
import Jaek.UI.FrpHandlers
import Jaek.UI.Input.Drags
import Jaek.UI.Views

import Reactive.Banana

import Data.Function as F
import Data.List
import Data.Ord

keynavActions :: Behavior TreeZip -> Event KeyVal -> Event Focus

keyActions :: 
  Behavior (Int,Int)
  -> Behavior ViewMap
  -> Behavior [DragEvent]
  -> Event KeyVal
  -> Event (TreeZip -> TreeZip)
keyActions bSz bVm bSels eKey =
  filterMaybes $ apply (keyactOnSelect <$> bSz <*> bVm <*> bSels) eKey

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
  reT :: [(a, [(b,c)])] -> [(a,b,c)]
  reT = concatMap (\(a, ys) -> map (\(b,c) -> (a,b,c)) ys)

  -- this slightly convoluted function removes overlapping segments
  -- in each channel.
  mkRegions tz = reT
                 . (fmap . fmap) removeOlaps
                 . map unT                      -- [(chn, [(off,dur)])]
                 . groupBy ((==) `F.on` fst)
                 . sortBy (comparing fst)
                 . concatMap (dragToRegions sz tz vm)
