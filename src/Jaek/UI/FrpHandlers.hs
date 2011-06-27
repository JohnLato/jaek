{-# LANGUAGE DeriveDataTypeable #-}

-- | general GTK FRP functions.  These may be split into a separate
-- module in the future...

module Jaek.UI.FrpHandlers (
  ClickType (..)
 ,ClickEvent (..)
 ,EventModifier (..)
 ,DragEvent (..)
 ,mapFilterE
 ,exposeEvents
 ,clickEvents
 ,releaseEvents
 ,motionEvents
 ,dragEvents
 ,genBSize
 ,genBDrag
)

where

import Graphics.UI.Gtk
import Jaek.Base
import Reactive.Banana as B
import Diagrams.Prelude ((<>))
import Data.Data

-- | Encapsulate information about a click
data ClickEvent = ClickE {
  clickType :: !ClickType
 ,clickMods :: [EventModifier]
 ,xPos :: !Double
 ,yPos :: !Double
 }
 deriving (Eq, Show, Ord, Data, Typeable)

-- | Encapsulate data about drag events
data DragEvent  = DragE {
  dragStart :: ClickEvent
 ,xDragEnd  :: !Double
 ,yDragEnd  :: !Double
 }
 deriving (Eq, Show, Data, Typeable)

data EventModifier
  = ShiftE
  | LockE
  | ControlE
  | AltE
  | Alt2E
  | Alt3E
  | Alt4E
  | Alt5E
  | Button1E
  | Button2E
  | Button3E
  | Button4E
  | Button5E
  | SuperE
  | HyperE
  | MetaE
  | ReleaseE
  | ModifierMaskE
  deriving (Eq, Ord, Show, Data, Typeable)

fromModifier :: Modifier -> EventModifier
fromModifier Shift = ShiftE
fromModifier Lock  = LockE
fromModifier Control = ControlE
fromModifier Alt     = AltE
fromModifier Alt2    = Alt2E
fromModifier Alt3    = Alt3E
fromModifier Alt4    = Alt4E
fromModifier Alt5    = Alt5E
fromModifier Button1 = Button1E
fromModifier Button2 = Button2E
fromModifier Button3 = Button3E
fromModifier Button4 = Button4E
fromModifier Button5 = Button5E
fromModifier Super   = SuperE
fromModifier Hyper   = HyperE
fromModifier Meta    = MetaE
fromModifier Release = ReleaseE
fromModifier ModifierMask = ModifierMaskE

-- | Check if a drag is valid, i.e. start and end points differ
checkDrag :: DragEvent -> Bool
checkDrag (DragE (ClickE _ _ cx cy) dx dy) = (cx /= dx) || (cy /= dy)

data ClickType = SingleC | DoubleC | TripleC | ReleaseC
  deriving (Eq, Show, Enum, Ord, Data, Typeable)

data DragAcc = None | Start ClickEvent | Full DragEvent

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

click2ClickType :: Click -> ClickType
click2ClickType SingleClick = SingleC
click2ClickType DoubleClick = DoubleC
click2ClickType TripleClick = TripleC
click2ClickType ReleaseClick = ReleaseC

mapFilterE :: (a -> b) -> (a -> Bool) -> Event a -> Event b
mapFilterE f p e = f <$> B.filter p e

exposeEvents :: WidgetClass w => w -> Prepare (Event ())
exposeEvents widget =
  fromAddHandler $ \k -> ignore $ widget `onExpose` const (k () >> return True)

clickEvents :: WidgetClass w => w -> Prepare (Event ClickEvent)
clickEvents widget =
  fromAddHandler $ \k -> ignore $ on widget buttonPressEvent $ tryEvent $ do
    click <- eventClick
    (x,y) <- eventCoordinates
    mods  <- eventModifier
    liftIO $ k $ ClickE (click2ClickType click) (map fromModifier mods) x y

releaseEvents :: WidgetClass w => w -> Prepare (Event ClickEvent)
releaseEvents widget =
  fromAddHandler $ \k -> ignore $ on widget buttonReleaseEvent $ tryEvent $ do
    click <- eventClick
    (x,y) <- eventCoordinates
    mods  <- eventModifier
    liftIO $ k $ ClickE (click2ClickType click) (map fromModifier mods) x y

motionEvents ::
  WidgetClass w
  => w
  -> Prepare (Event ([EventModifier], Double, Double))
motionEvents widget =
 fromAddHandler $ \k -> ignore $ on widget motionNotifyEvent $ tryEvent $ do
    (x,y) <- eventCoordinates
    mods  <- eventModifier
    liftIO $ k (fmap fromModifier mods, x, y)

-- | Generate drag events.  The input Event ClickEvent should have both clicks
-- and releases, such as
-- 
-- > clicks <- clickEvents w
-- > releases <- releaseEvents w
-- > let drags = dragEvents $ clicks <> releases
dragEvents :: Event ClickEvent -> Event DragEvent
dragEvents es = B.filter checkDrag . mapFilterE fromAcc fullAcc $
  accumE None (addClick <$> es)

-- | generate a behavior of the current drag event.  This should work
-- for any EventMask, even if PointerMotionMask is on, because it filters
-- all events when a button isn't pressed.
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
-- In @Prepare@ because this is uses an internal @Event@.
genBSize :: WidgetClass w => w -> Prepare (Behavior (Int, Int))
genBSize widget = do
   eSize <- e
   sz <- liftIO $ widgetGetSize widget
   return $ stepper sz eSize
 where
   e = fromAddHandler $ \k -> ignore $ on widget sizeAllocate $
     \(Rectangle _ _ width height) -> k (width,height)
