module Jaek.UI.Controllers.Edit1 (
  editCtrl1
)

where

import Graphics.UI.Gtk
import Jaek.SegmentOlaps
import Jaek.Tree
import Jaek.UI.AllSources
import Jaek.UI.Controllers.Base
import Jaek.UI.Controllers.Drags
import Jaek.UI.FrpHandlers
import Jaek.UI.Logic.SelectionMediators
import Jaek.UI.Views

import Reactive.Banana
import Diagrams.Prelude ((<>))

import Data.Function as F
import Data.List
import Data.Ord

editCtrl1 :: 
  Discrete (Int,Int)
  -> Discrete ViewMap
  -> Controller [(NodeRef, [ClipRef])]
  -> Controller [DragEvent]
  -> Sources
  -> Event ClickEvent
  -> Event ClickEvent
  -> Event KeyVal
  -> Event MotionEvent
  -> Controller ()
editCtrl1 bSz bVm clipCtrl selCtrl sources clicks releases keys motions =
  nullController { dActive = isActive
                  ,dState      = pure ()
                  ,clickPass   = clicks
                  ,releasePass = releases
                  ,keysPass    = passFilter keys isActive passkeys
                  ,motionsPass = motions
                  ,eZipChange  = zChng }
 where
  -- active with current selection
  isActive = (\sAct sel -> sAct && not (null sel))
             <$> dActive selCtrl <*> dState selCtrl
  (passkeys, zChng) = splitEithers $
            (keyactOnSelect <$> bSz <*> bVm <*> dState clipCtrl
                                    <*> dState selCtrl)
            <@> filterApply (const <$> value isActive)
                            (keys <> eventSourceMods sources)

eventSourceMods
  :: Sources
  -> Event KeyVal
eventSourceMods sources =
  (   keyFromName "m" <$ getMuteSource   sources)
  <> (keyFromName "d" <$ getDeleteSource sources)
  <> (keyFromName "i" <$ getInsertSource sources)

keyactOnSelect ::
  (Int,Int)
  -> ViewMap
  -> [(NodeRef, [ClipRef])]
  -> [DragEvent]
  -> KeyVal
  -> Either KeyVal (TreeZip -> TreeZip)
keyactOnSelect sz vm src sels key
  | sels == []                = Left key
  | keyToChar key == Just 'm' = Right $ \tz ->
                                  mkMute (mkRegions sz tz vm sels) tz
  | keyToChar key == Just 'd' = Right $ \tz ->
                                  mkCut (mkRegions sz tz vm sels) tz
    -- for inserts, only insert into currently selected channels.
    -- if  the selection to paste has multiple channels which begin at
    -- the same point, paste them all into the currently selected channels
  | keyToChar key == Just 'i' = Right $ \tz ->
                                  insertSels src (mkRegions sz tz vm sels) tz
  | otherwise                 = Left key

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
-- and also converts from the start/end representation to offset/dur
-- representation
reT :: Num b => [(a, [(b,b)])] -> [(a,b,b)]
reT = concatMap (\(a, ys) -> map (\(b,c) -> (a,b,c-b)) ys)
