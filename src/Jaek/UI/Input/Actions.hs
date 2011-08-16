module Jaek.UI.Input.Actions (
  keyActions
 ,keynavActions
)

where

import Graphics.UI.Gtk
import Jaek.SegmentOlaps
import Jaek.Tree
import Jaek.UI.Controllers
import Jaek.UI.Focus
import Jaek.UI.FrpHandlers
import Jaek.UI.Input.Drags
import Jaek.UI.Views

import Reactive.Banana

import Data.Function as F
import Data.List
import Data.Ord

keynavActions ::
  Behavior Focus
  -> Discrete TreeZip
  -> Event KeyVal
  -> Controller ()
keynavActions bFoc bZip eKey = Controller (pure True) (pure ()) defaultPred defaultPred keyPred defaultPred eFocChange never never never never
 where
  keyPred _ kv = maybe True (const False) $ keynav undefined undefined kv
  eFocChange = filterMaybes $ (keynav <$> bFoc <*> value bZip) <@> eKey

keynav :: Focus -> TreeZip -> KeyVal -> Maybe Focus
keynav _foc _tz keyval
  | keyval == 65307 = Just Nothing
  | otherwise       = Nothing

keyActions :: 
  Discrete (Int,Int)
  -> Discrete ViewMap
  -> Discrete [DragEvent]
  -> Event KeyVal
  -> Event (TreeZip -> TreeZip)
keyActions bSz bVm bSels eKey =
  filterMaybes $ (keyactOnSelect <$> bSz <*> bVm <*> bSels) <@> eKey

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
