{-# LANGUAGE DeriveDataTypeable #-}

-- | general GTK FRP functions.  These may be split into a separate
-- module in the future...

module Jaek.UI.FrpHandlers (
  ClickType (..)
 ,ClickEvent (..)
 ,mapFilterE
 ,exposeEvents
 ,clickEvents
 ,releaseEvents
 ,dragEvents
 ,genBSize
)

where

import Graphics.UI.Gtk
import Jaek.Base
import Reactive.Banana as B
import Data.Bits
import Data.Data

-- | Encapsulate information about a click
data ClickEvent = ClickE {
  clickType :: !ClickType
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

-- | Check if a drag is valid, i.e. start and end points differ
checkDrag :: DragEvent -> Bool
checkDrag (DragE (ClickE _ cx cy) dx dy) = (cx /= dx) || (cy /= dy)

data ClickType = SingleC | DoubleC | TripleC | ReleaseC
  deriving (Eq, Show, Enum, Ord, Data, Typeable)

data DragAcc = None | Start ClickEvent | Full DragEvent

addClick :: ClickEvent -> DragAcc -> DragAcc
addClick (ClickE ReleaseC x y) (Start e) = Full $ DragE e x y
addClick (ClickE ReleaseC _ _) _ = None
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
    liftIO $ k $ ClickE (click2ClickType click) x y

releaseEvents :: WidgetClass w => w -> Prepare (Event ClickEvent)
releaseEvents widget =
  fromAddHandler $ \k -> ignore $ on widget buttonReleaseEvent $ tryEvent $ do
    click <- eventClick
    (x,y) <- eventCoordinates
    liftIO $ k $ ClickE (click2ClickType click) x y

-- | Generate drag events.  The input Event ClickEvent should have both clicks
-- and releases, such as
-- 
-- > clicks <- clickEvents w
-- > releases <- releaseEvents w
-- > let drags = dragEvents $ clicks <> releases
dragEvents :: Event ClickEvent -> Event DragEvent
dragEvents es = B.filter checkDrag . mapFilterE fromAcc fullAcc $
  accumE None (addClick <$> es)

-- | Create a behavior of the size of a widget.
-- 
-- In @Prepare@ because this is uses an internal @Event@.
genBSize :: WidgetClass w => w -> Prepare (Behavior (Int, Int))
genBSize widget = do
   eSize <- e
   sz <- liftIO $ widgetGetSize widget
   return $ accumB sz (const <$> eSize)
 where
   e = fromAddHandler $ \k -> ignore $ on widget sizeAllocate $
     \(Rectangle _ _ width height) -> k (width,height)
