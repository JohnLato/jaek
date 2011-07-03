{-# LANGUAGE TupleSections #-}

module Jaek.UI.Input.Drags (
  bSelection
 ,dragToRegions
)

where

import Jaek.Base
import Jaek.Tree
import Jaek.UI.Focus
import Jaek.UI.FrpHandlers
import Jaek.UI.Views

import Reactive.Banana  as FRP
import Diagrams.Prelude as D
import Data.Record.Label

import Control.Arrow

-- The current selection is a Behavior [DragEvent].  There are two components,
-- the current drag event, and any prior selected regions (if the current event
-- is additive).  The selections are cleared by a non-additive click
-- event outside a selected area.
-- 
-- when a click occurs within the current selection, it should be ignored.
bSelection ::
  Behavior (Int,Int)
  -> Behavior Focus
  -> Behavior TreeZip
  -> Event ClickEvent    -- ^ clicks
  -> Event ClickEvent    -- ^ releases
  -> Event DragEvent
  -> Event ([EventModifier], Double, Double)
  -> Behavior [DragEvent]
bSelection bSize bFocus bZip clicks releases drags motions =
  (\xs mx -> maybe xs (:xs) mx) <$> bS1 <*> bS2
 where
  ff sel clk = not (any (\drg ->
     contains' (fromCorners (P $ getL xyStart drg)
                            (P $ getL xyEnd drg))
       $ P $ getL xyClick clk) sel)
  ff' = (\sel clk -> not (clickIsAdditive clk) && ff sel clk) <$> bS1
  bS1 = (\w f z s -> map (channelizeDrag w f z) s) <$> bSize
          <*> bFocus <*> bZip
          <*> bCurSelection (filterApply bMask drags)
                            (() <$ filterApply ff' clicks)
  bS2 = (\w f z s -> fmap (channelizeDrag w f z) s)  <$> bSize
          <*> bFocus <*> bZip
          <*> genBDrag filtCurClk (filterApply bMask motions)
  -- for genBDrag's input, if we filter a click, we also want to filter the 
  -- release.  Annotate each click with a bool, true iff we keep the click,
  -- otherwise false
  annClicks = FRP.apply ((\sel clk -> (ff sel clk, clk)) <$> bS1) clicks
  bMask :: Behavior (u -> Bool)
  bMask = stepper (const False) $ (const . fst <$> annClicks)
  filtCurClk = mapFilterE snd fst annClicks <> filterApply bMask releases


-- | check if a drag event should be added to current selection (shift-drag)
-- or replace it.
dragIsAdditive :: DragEvent -> Bool
dragIsAdditive = clickIsAdditive . getL dragStart

clickIsAdditive :: ClickEvent -> Bool
clickIsAdditive = any (== ShiftE) . getL clickMods

-- | The (X,Y) coordinates of a DragEvent need to be adjusted to match
-- the channels in a WaveView.
channelizeDrag :: (Int, Int) -> Focus -> TreeZip -> DragEvent -> DragEvent
channelizeDrag (_, ySz) focus zp drg
  | isTree focus = drg
  | nc <= 1      = drg
  | otherwise = modL dragYs ((inf *** inf) >>> adjf >>> (outf *** outf)) drg
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
   $ getL dragYs drg
 where
  t  = hole zp
  WaveView off dur = getView vm t
  nc = liftT numChans t
  (xStart,xEnd) = (x2sc . uncurry min &&& x2sc . uncurry max) $ getL dragXs drg
  x2sc x = off + floor (fI dur * (x / fI xSz))
  chnBorder y = (round $ fI nc * (y / fI ySz)) :: Int

bCurSelection :: Event DragEvent -> Event () -> Behavior [DragEvent]
bCurSelection eDrags eClear =
  accumB [] $ (dragAcc <$> eDrags) <> (clearAcc <$> eClear)
 where
  dragAcc drag acc
    | dragIsAdditive drag = drag:acc
    | otherwise           = [drag]
  clearAcc () _           = []
