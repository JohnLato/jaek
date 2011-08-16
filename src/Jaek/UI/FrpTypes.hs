{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeOperators #-}

-- | general GTK FRP functions.  These may be split into a separate
-- module in the future...

module Jaek.UI.FrpTypes

where

import Graphics.UI.Gtk
import Reactive.Banana as B
import Data.Data

import Data.Label

type MotionEvent = ([EventModifier], Double, Double)

data ClickType = SingleC | DoubleC | TripleC | ReleaseC
  deriving (Eq, Show, Enum, Ord, Data, Typeable)

-- | Encapsulate information about a click
data ClickEvent = ClickE {
  _clickType :: !ClickType
 ,_clickMods :: [EventModifier]
 ,_xPos :: !Double
 ,_yPos :: !Double
 }
 deriving (Eq, Show, Ord, Data, Typeable)

-- | Encapsulate data about drag events
data DragEvent  = DragE {
  _dragStart :: ClickEvent
 ,_xDragEnd  :: !Double
 ,_yDragEnd  :: !Double
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

data DragAcc = None | Start ClickEvent | Full DragEvent

click2ClickType :: Click -> ClickType
click2ClickType SingleClick = SingleC
click2ClickType DoubleClick = DoubleC
click2ClickType TripleClick = TripleC
click2ClickType ReleaseClick = ReleaseC

isClick :: ClickEvent -> Bool
isClick = not . isRelease

isRelease :: ClickEvent -> Bool
isRelease (ClickE ReleaseC _ _ _) = True
isRelease _                       = False

$(mkLabels [''ClickEvent, ''DragEvent])
