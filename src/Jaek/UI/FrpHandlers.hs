{-# LANGUAGE TypeOperators #-}

-- | general GTK FRP functions.  These may be split into a separate
-- module in the future...

module Jaek.UI.FrpHandlers (
  xyClick
 ,xyStart
 ,xyEnd
 ,dragYs
 ,mapFilterE
 ,exposeEvents
 ,keypressEvents
 ,clickEvents
 ,releaseEvents
 ,motionEvents
 ,dragEvents
 ,genBSize
 ,genBDrag
 ,module F
)

where

import Prelude hiding ((.))

import Graphics.UI.Gtk
import Jaek.Base
import Jaek.UI.FrpTypes as F
import Reactive.Banana as B
import Diagrams.Prelude ((<>))

import Data.Record.Label

import Control.Category

-- | A lense for the (X,Y) coordinates of a Click
xyClick :: ClickEvent :-> (Double, Double)
xyClick = Lens $ (,) <$> fst `for` xPos <*> snd `for` yPos

xyStart :: DragEvent :-> (Double,Double)
xyStart = xyClick . dragStart

xyEnd :: DragEvent :-> (Double, Double)
xyEnd = Lens $ (,) <$> fst `for` xDragEnd <*> snd `for` yDragEnd

-- | A lens on the @(yStart, yEnd)@ values of a @DragEvent@.  Note that 
-- start and end refer to the start and end clicks.
dragYs :: DragEvent :-> (Double, Double)
dragYs = Lens $ (,) <$> fst `for` (yPos . dragStart) <*> snd `for` yDragEnd

releaseFromDrag :: DragEvent -> ClickEvent
releaseFromDrag (DragE strt x y) = ClickE ReleaseC (getL clickMods strt) x y

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

mapFilterE :: (a -> b) -> (a -> Bool) -> Event a -> Event b
mapFilterE f p e = f <$> filterE p e

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
    liftIO $ k kv

clickEvents :: WidgetClass w => w -> NetworkDescription (Event ClickEvent)
clickEvents widget = event1 $ \k ->
  ignore $ on widget buttonPressEvent $ tryEvent $ do
    click <- eventClick
    (x,y) <- eventCoordinates
    mods  <- eventModifier
    liftIO $ k $ ClickE (click2ClickType click) (map fromModifier mods) x y

releaseEvents :: WidgetClass w => w -> NetworkDescription (Event ClickEvent)
releaseEvents widget = event1 $ \k ->
  ignore $ on widget buttonReleaseEvent $ tryEvent $ do
    click <- eventClick
    (x,y) <- eventCoordinates
    mods  <- eventModifier
    liftIO $ k $ ClickE (click2ClickType click) (map fromModifier mods) x y

motionEvents ::
  WidgetClass w
  => w
  -> NetworkDescription (Event ([EventModifier], Double, Double))
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
genBDrag ::
  Event ClickEvent
  -> Event ([EventModifier], Double, Double) 
  -> Behavior (Maybe DragEvent)
genBDrag clicks motions = accumB Nothing $ (cf' <$> clicks) <> (mf <$> filtms)
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
genBSize :: WidgetClass w => w -> NetworkDescription (Behavior (Int, Int))
genBSize widget = do
   eSize <- e
   sz <- liftIO $ widgetGetSize widget
   return $ stepper sz eSize
 where
   e = event1 $ \k -> ignore $ on widget sizeAllocate $
     \(Rectangle _ _ width height) -> k (width,height)
