{-# LANGUAGE TypeOperators
            ,TypeSynonymInstances
            ,FlexibleInstances #-}

-- | general GTK FRP functions.  These may be split into a separate
-- module in the future...

module Jaek.UI.FrpHandlers (
  -- * Types and Classes
  HasXY (..)
  -- * Lenses
 ,xyClick
 ,xyStart
 ,xyEnd
 ,xyMotion
 ,dragXs
 ,dragYs
  -- * Other stuff
 ,mapFilterE
 ,sampleD
 ,filterMaybes
 ,splitEithers
 ,exposeEvents
 ,keypressEvents
 ,clickEvents
 ,releaseEvents
 ,motionEvents
 ,dragEvents
 ,genDSize
 ,genDDrag
 ,module F
 ,KeyVal       -- from Graphics.UI.Gtk
)

where

import Prelude hiding ((.))

import Graphics.UI.Gtk
import Jaek.Base
import Jaek.UI.FrpTypes as F
import Reactive.Banana as B
import Diagrams.Prelude ((<>))

import Data.Label as L

import Control.Category
import Data.Maybe

-- | A lense for the (X,Y) coordinates of a Click
xyClick :: ClickEvent :-> (Double, Double)
xyClick = Lens $ (,) <$> fst `for` xPos <*> snd `for` yPos

xyStart :: DragEvent :-> (Double,Double)
xyStart = xyClick . dragStart

xyMotion :: MotionEvent :-> (Double, Double)
xyMotion = lens (\(_,x,y) -> (x,y)) (\(x,y) (t,_,_) -> (t,x,y))

xyEnd :: DragEvent :-> (Double, Double)
xyEnd = Lens $ (,) <$> fst `for` xDragEnd <*> snd `for` yDragEnd

dragXs :: DragEvent :-> (Double, Double)
dragXs = Lens $ (,) <$> fst `for` (xPos . dragStart) <*> snd `for` xDragEnd

-- | A lens on the @(yStart, yEnd)@ values of a @DragEvent@.  Note that 
-- start and end refer to the start and end clicks.
dragYs :: DragEvent :-> (Double, Double)
dragYs = Lens $ (,) <$> fst `for` (yPos . dragStart) <*> snd `for` yDragEnd

-- ----------------------------------------
-- A few classes for working with clicks and drags

-- | Things with an (x,y) coordinate.
class HasXY e where
  getXY :: e :-> (Double,Double)

instance HasXY ClickEvent where
  getXY = xyClick

-- | For DragEvent, return the starting (x,y) coordinates
instance HasXY DragEvent where
  getXY = xyStart

instance HasXY MotionEvent where
  getXY = xyMotion

releaseFromDrag :: DragEvent -> ClickEvent
releaseFromDrag (DragE strt x y) = ClickE ReleaseC (L.get clickMods strt) x y

addClick :: ClickEvent -> DragAcc -> DragAcc
addClick (ClickE ReleaseC _ x y) (Start e) = Full $ DragE e x y
addClick (ClickE ReleaseC _ _ _) _ = None
addClick e _ = Start e

fullAcc :: DragAcc -> Bool
fullAcc (Full _) = True
fullAcc _        = False

fromAcc :: DragAcc -> DragEvent
fromAcc (Full d) = d
fromAcc _ = undefined

sampleD :: Discrete a -> Event () -> Event a
sampleD dscr e = (const <$> dscr) <@> e

mapFilterE :: (a -> b) -> (a -> Bool) -> Event a -> Event b
mapFilterE f p e = f <$> filterE p e

filterMaybes :: Event (Maybe a) -> Event a
filterMaybes e = fromJust <$> filterE isJust e

splitEithers :: Event (Either l r) -> (Event l, Event r)
splitEithers e = ((\(Left x) -> x) <$> filterE isLeft e
                 ,(\(Right x) -> x) <$> filterE isRight e)

isRight :: Either l r -> Bool
isRight (Right _) = True
isRight _        = False

isLeft :: Either l r -> Bool
isLeft (Left _) = True
isLeft _        = False

event1 :: Typeable a => ((a -> IO ()) -> IO ()) -> NetworkDescription (Event a)
event1 k = do
  (addHandler, runHandlers) <- liftIO newAddHandler
  liftIO $ k runHandlers
  fromAddHandler addHandler

exposeEvents :: WidgetClass w => w -> NetworkDescription (Event ())
exposeEvents widget = event1 $ \k ->
  ignore $ widget `onExpose` const (k () >> return True)

keypressEvents :: WidgetClass w => w -> NetworkDescription (Event KeyVal)
keypressEvents widget = event1 $ \k ->
  ignore $ on widget keyPressEvent $ tryEvent $ do
    kv <- eventKeyVal
    liftIO $ print $ "got KeyVal: " ++ show kv
    liftIO $ k kv

clickEvents :: WidgetClass w => w -> NetworkDescription (Event ClickEvent)
clickEvents = clickEvents' buttonPressEvent

releaseEvents :: WidgetClass w => w -> NetworkDescription (Event ClickEvent)
releaseEvents = clickEvents' buttonReleaseEvent

clickEvents'
  :: WidgetClass w
   => Signal w (EventM EButton Bool)
   -> w
   -> NetworkDescription (Event ClickEvent)
clickEvents' evt widget = event1 $ \k ->
  ignore $ on widget evt $ tryEvent $ do
    click <- eventClick
    (x,y) <- eventCoordinates
    mods  <- eventModifier
    liftIO $ k $ ClickE (click2ClickType click) (map fromModifier mods) x y

motionEvents ::
  WidgetClass w
  => w
  -> NetworkDescription (Event MotionEvent)
motionEvents widget = event1 $ \k ->
 ignore $ on widget motionNotifyEvent $ tryEvent $ do
    (x,y) <- eventCoordinates
    mods  <- eventModifier
    liftIO $ k (fmap fromModifier mods, x, y)

-- | Generate drag events.  The input Event ClickEvent should have both clicks
-- and releases, such as
-- 
-- > clicks <- clickEvents w
-- > releases <- releaseEvents w
-- > let drags = fst <$> dragEvents $ clicks <> releases
-- 
-- The second returned parameter are clicks releases which *do not*
-- include DragEvent-ending releases.
dragEvents :: Event ClickEvent -> (Event DragEvent, Event ClickEvent)
dragEvents es =
  (filterE checkDrag allDrags
  ,mapFilterE releaseFromDrag (not . checkDrag) allDrags)
 where
  allDrags = mapFilterE fromAcc fullAcc $ accumE None (addClick <$> es)

-- | generate a behavior of the current drag event.  This should work
-- for any EventMask, even if PointerMotionMask is on, because it filters
-- all events when a mouse button isn't pressed.
genDDrag ::
  Event ClickEvent
  -> Event ([EventModifier], Double, Double) 
  -> Discrete (Maybe DragEvent)
genDDrag clicks motions = accumD Nothing $ (cf' <$> clicks) <> (mf <$> filtms)
 where
  cf (ClickE ReleaseC _ _ _) = False
  cf _                     = True
  inPress = stepper False $ cf <$> clicks
  filtms = filterApply (const <$> inPress) motions
  cf' (ClickE ReleaseC _ x y) (Just (DragE c _ _)) = Just $ DragE c x y
  cf' c@(ClickE _ _ x y)      _                    = Just $ DragE c x y
  cf' _                       _                    = Nothing
  mf (_,x,y)                  (Just (DragE c _ _)) = Just $ DragE c x y
  mf _                        _                    = Nothing

-- | Create a behavior of the size of a widget.
-- 
-- In @NetworkDescription@ because this is uses an internal @Event@.
genDSize :: WidgetClass w => w -> NetworkDescription (Discrete (Int, Int))
genDSize widget = do
   eSize <- e
   sz <- liftIO $ widgetGetSize widget
   return $ stepperD sz eSize
 where
   e = event1 $ \k -> ignore $ on widget sizeAllocate $
     \(Rectangle _ _ width height) -> k (width,height)
