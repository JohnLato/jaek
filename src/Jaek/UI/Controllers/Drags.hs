{-# LANGUAGE TupleSections #-}

module Jaek.UI.Controllers.Drags (
  selectCtrl
 ,dragToRegions
)

where

import Jaek.Base
import Jaek.Tree
import Jaek.UI.Controllers.Base
import Jaek.UI.Focus
import Jaek.UI.FrpHandlers
import Jaek.UI.Render.Overlays
import Jaek.UI.Views

import Reactive.Banana  as FRP
import Diagrams.Prelude as D
import Data.Label as L
import qualified Data.Tuple.Update as T

import Control.Arrow
import Control.Monad.Identity

-- The current selection is a Discrete [DragEvent].  There are two components,
-- the current drag event, and any prior selected regions (if the current event
-- is additive).  The selections are cleared by a non-additive click
-- event outside a selected area.
-- 
-- when a click occurs within the current selection, it should be ignored.
-- 
-- This controller is only active when a Wave is in focus.
selectCtrl
  :: Discrete (Int,Int)
  -> Discrete Focus
  -> Discrete TreeZip
  -> Event ClickEvent    -- ^ clicks
  -> Event ClickEvent    -- ^ releases
  -> Event DragEvent
  -> Event MotionEvent
  -> Event KeyVal
  -> Controller [DragEvent]
selectCtrl bSize bFocus bZip clicks releases drags motions keys =
  nullController { dActive     = isActive
                  ,dState      = dSel
                  ,keysPred    = keypred
                  ,bDiagChange = compositeSelection <$> value dSel
                  ,refreshTrig = () <$ changes dSel }
 where
  isActive     = isWave <$> bFocus
  filterActive = filterApply (const <$> value isActive)
  filtOnPos :: HasXY a => Event a -> Event a
  filtOnPos    = filterApply
                   ((\sels drag ->
                        not (any (\drg -> contains'
                          (fromCorners (P $ L.get xyStart drg)
                                       (P $ L.get xyEnd drg))
                          $ P $ L.get getXY drag) sels) )
                    <$> value dSel)
  dSel         = combiner
                 <$> accumD (Nothing, []) (eDrags <> eCurDrag <> breaks)
  -- the current selection is made of two components:
  -- 1.  The current drag region, if it's additive (e.g. shift-drag)
  -- 2.  Everything which is already selected
  combiner (Nothing, rest)   = rest
  combiner (Just this, rest)
    | dragIsAdditive this    = this:rest
    | otherwise              = [this]
  -- need to create Event ((Maybe DE, [DE]) -> (Maybe DE, [DE]) )
  curDrag  = genDDrag (filtOnPos . filterActive $ clicks <> releases)
                      (filterActive $ motions)
  eCurDrag = T.upd1 <$> (dChannelize <@> changes curDrag)
  eDrags   = (\(Identity drag) ->
                  if dragIsAdditive drag
                    then second (drag: )
                    else second (const [drag]))
             <$> (dChannelize <@> (Identity <$> filtOnPos (filterActive drags)))
  dChannelize :: (Functor f) => Discrete (f DragEvent -> f DragEvent)
  dChannelize = (\w f z s -> fmap (channelizeDrag w f z) s)
                  <$> bSize <*> bFocus <*> bZip
  breaks = filterMaybes (breakKeyF <$> keys)
  keypred [] keyval = False
  keypred _  keyval = maybe False (const True) $ breakKeyF keyval
  -- breakKeyF :: KeyVal -> Maybe ((Maybe DE, [DE]) ->  (Maybe DE, [DE]))
  breakKeyF keyval
    | keyval == 65307 = Just (const (Nothing, []))
    | otherwise       = Nothing

-- | check if a drag event should be added to current selection (shift-drag)
-- or replace it.
dragIsAdditive :: DragEvent -> Bool
dragIsAdditive = clickIsAdditive . L.get dragStart

clickIsAdditive :: ClickEvent -> Bool
clickIsAdditive = any (== ShiftE) . L.get clickMods

-- | The (X,Y) coordinates of a DragEvent need to be adjusted to match
-- the channels in a WaveView.
channelizeDrag :: (Int, Int) -> Focus -> TreeZip -> DragEvent -> DragEvent
channelizeDrag (_, ySz) focus zp drg
  | isTree focus = drg
  | nc <= 1      = drg
  | otherwise = modify dragYs ((inf *** inf) >>> adjf >>> (outf *** outf)) drg
 where
  nc = liftT numChans $ hole zp
  adjf (s,e) = if e >= s then (floor s, ceiling e) else (ceiling s, floor e)
  ySz' = fI ySz :: Double
  nc'  = fI nc :: Double
  inf y = nc' * (y / ySz')
  outf :: Int -> Double
  outf y = ySz' * (fI y / nc')

-- | usually I use @(Int,SampleCount, SampleCount)@ for the region type,
-- but here it's nested tuples to facilitate further processing
dragToRegions ::
  (Int, Int)
  -> TreeZip
  -> ViewMap
  -> DragEvent
  -> [(Int, (SampleCount, SampleCount))]
dragToRegions (xSz, ySz) zp vm drg =
  map (,(xStart,xEnd))
   . uncurry enumFromTo
   . (uncurry min &&& (subtract 1 . uncurry max))
   . (chnBorder *** chnBorder)
   $ L.get dragYs drg
 where
  t  = hole zp
  WaveView off dur = getView vm t
  nc = liftT numChans t
  (xStart,xEnd) = (x2sc . uncurry min &&& x2sc . uncurry max) $ L.get dragXs drg
  x2sc x = off + floor (fI dur * (x / fI xSz))
  chnBorder y = round $ fI nc * (y / fI ySz) :: Int
