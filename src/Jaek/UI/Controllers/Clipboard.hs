{-# LANGUAGE TupleSections #-}

module Jaek.UI.Controllers.Clipboard (
  clipboardCtrl
)

where

import Graphics.UI.Gtk
import Jaek.Base
import Jaek.SegmentOlaps
import Jaek.Tree
import Jaek.UI.AllSources
import Jaek.UI.Controllers.Base
import Jaek.UI.Controllers.Drags
import Jaek.UI.FrpHandlers
import Jaek.UI.Views

import Reactive.Banana
import Diagrams.Prelude ((<>))

import Data.Function as F
import Data.List
import Data.Ord

clipboardCtrl :: 
  Discrete (Int,Int)
  -> Discrete ViewMap
  -> Discrete TreeZip
  -> Controller [DragEvent]
  -> Sources
  -> Event ClickEvent
  -> Event ClickEvent
  -> Event KeyVal
  -> Event MotionEvent
  -> Controller [(NodeRef, [ClipRef])]
clipboardCtrl bSz bVm dTz selCtrl sources clicks releases keys motions =
  nullController { dActive     = isActive
                  ,dState      = accumD [] addToClip
                  ,clickPass   = clicks
                  ,releasePass = releases
                  ,keysPass    = keys
                  ,motionsPass = motions }
 where
  -- active with current selection
  isActive = (\sAct sel -> sAct && not (null sel))
             <$> dActive selCtrl <*> dState selCtrl
  addToClip = snd <$> splitEithers $
               (keyactOnSelect <$> dTz <*> bSz <*> bVm <*> dState selCtrl)
               <@> filterApply (const <$> value isActive)
                               (keys <> eventSourceMods sources)

eventSourceMods
  :: Sources
  -> Event KeyVal
eventSourceMods sources =
  (   keyFromName "c" <$ getCopySource sources)
  <> (keyFromName "d" <$ getDeleteSource sources)

keyactOnSelect ::
  TreeZip
  -> (Int,Int)
  -> ViewMap
  -> [DragEvent]
  -> KeyVal
  -> Either KeyVal ([(NodeRef, [ClipRef])] -> [(NodeRef, [ClipRef])])
keyactOnSelect tz sz vm sels key
  | sels == []                = Left key
  | keyToChar key == Just 'c' = Right (clip : )
  | keyToChar key == Just 'd' = Right (clip : )
  | otherwise                 = Left key
 where clip = (AbsPath $ getPath tz, mkRegions sz tz vm sels)
  
-- this slightly convoluted function removes overlapping segments
-- in each channel.
mkRegions :: (Int, Int) -> TreeZip -> ViewMap -> [DragEvent] -> [ClipRef]
mkRegions sz tz vm = reT
  . (fmap . fmap) removeOlaps
  . map unT                      -- [(chn, [(off,dur)])]
  . groupBy ((==) `F.on` fst)
  . sortBy (comparing fst)
  . concatMap (dragToRegions sz tz vm)

unT :: [(a,b)] -> (a,[b])
unT xs = let ix = fst $ head xs
         in (ix, map snd xs)

-- reT converts the channelized data back to unchannelized tuples,
reT :: [(a, [(b,c)])] -> [(a,b,c)]
reT = concatMap (\(a, ys) -> map (\(b,c) -> (a,b,c)) ys)

