{-# LANGUAGE DeriveDataTypeable #-}

module Jaek.UI.FrpHandlers (
  ClickType (..)
 ,ClickEvent (..)
 ,exposeEvents
 ,clickEvents
 ,genBSize
)

where

import Graphics.UI.Gtk
import Jaek.Base
import Reactive.Banana
import Data.Data

-- | Encapsulate information about a click
data ClickEvent = ClickE {
  clickType :: ClickType
 ,xPos :: Double
 ,yPos :: Double
 }
 deriving (Eq, Show, Ord, Data, Typeable)

data ClickType = SingleC | DoubleC | TripleC | ReleaseC
  deriving (Eq, Show, Enum, Ord, Data, Typeable)

click2ClickType :: Click -> ClickType
click2ClickType SingleClick = SingleC
click2ClickType DoubleClick = DoubleC
click2ClickType TripleClick = TripleC
click2ClickType ReleaseClick = ReleaseC

exposeEvents :: WidgetClass w => w -> Prepare (Event ())
exposeEvents widget =
  fromAddHandler $ \k -> ignore $ widget `onExpose` const (k () >> return True)


clickEvents :: WidgetClass w => w -> Prepare (Event ClickEvent)
clickEvents widget =
  fromAddHandler $ \k -> ignore $ on widget buttonPressEvent $ tryEvent $ do
    click <- eventClick
    (x,y) <- eventCoordinates
    liftIO $ k $ ClickE (click2ClickType click) x y

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
