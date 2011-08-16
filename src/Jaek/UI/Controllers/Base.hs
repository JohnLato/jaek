{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, RankNTypes #-}

-- Define the Controller spec, as well as the basic null controller
module Jaek.UI.Controllers.Base (
  Controller (..)
 ,ControlSet
 -- * Functions
 -- ** ControlSet functions
 ,addController
 ,eFocChangeSet
 -- ** individual controller functions
 ,nullController
 ,defaultPred
 ,filterController
)

where

import Graphics.UI.Gtk
import Jaek.Tree
import Jaek.UI.FrpTypes
import Jaek.UI.Focus
import Jaek.UI.Views

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Reactive.Banana
import Data.Data

import Data.Monoid (Monoid (..))

type Pred st e = st -> e -> Bool

-- | A controller reacts to inputs and produces some sort of output.
--   The filters pass along any values to which a controller doesn't respond.
data Controller st = Controller {
   dActive     :: Discrete Bool
  ,dState      :: Discrete st
  ,clickPred   :: Pred st ClickEvent
  ,releasePred :: Pred st ClickEvent
  ,keysPred    :: Pred st KeyVal
  ,motionsPred :: Pred st MotionEvent
  ,eFocChange  :: Event Focus
  ,eZipChange  :: Event (TreeZip -> TreeZip)
  ,eDiagChange :: Event (Diagram Cairo R2 -> Diagram Cairo R2)
  ,eViewChange :: Event (ViewMap -> ViewMap)
  ,response    :: Event (IO ())
  }
  deriving (Typeable)

data EControl = forall st. EControl (Controller st)

type ControlSet = [EControl]

extract :: (forall st. Controller st -> b) -> EControl -> b
extract fn (EControl ctrl) = fn ctrl

addController :: Controller st -> ControlSet -> ControlSet
addController ctrl set = EControl ctrl : set

-- | A controller which doesn't do anything, and passes through all events.
nullController :: Controller ()
nullController =
  Controller (pure False)
             (pure ())
             defaultPred
             defaultPred
             defaultPred
             defaultPred
             never
             never
             never
             never
             never

-- | Default predicate, pass through everything.
defaultPred :: Pred a b
defaultPred _ _ = True

filterController
  :: Controller a
  -> Event ClickEvent
  -> Event ClickEvent
  -> Event KeyVal
  -> Event MotionEvent
  -> (Event ClickEvent, Event ClickEvent, Event KeyVal, Event MotionEvent)
filterController ctrl clicks releases keys motions =
  (oEvents (clickPred ctrl) clicks
  ,oEvents (releasePred ctrl) releases
  ,oEvents (keysPred ctrl) keys
  ,oEvents (motionsPred ctrl) motions)
 where
  oEvents pfunc inEvents = filterApply (value $ predB pfunc) inEvents
  predB pFunc = (\isActive -> if isActive then pFunc else \ _ _ -> True)
                <$> dActive ctrl <*> dState ctrl

filterControlSet
  :: ControlSet
  -> Event ClickEvent
  -> Event ClickEvent
  -> Event KeyVal
  -> Event MotionEvent
  -> (Event ClickEvent, Event ClickEvent, Event KeyVal, Event MotionEvent)
filterControlSet set clicks releases keys motions = foldr fn (clicks, releases, keys, motions) set
 where
  fn ectr (c, r, k, m) = extract (\ctrl -> filterController ctrl c r k m) ectr

eFocChangeSet :: ControlSet -> Event Focus
eFocChangeSet set = mconcat $ map (extract eFocChange) set
