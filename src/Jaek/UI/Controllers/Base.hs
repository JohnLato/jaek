{-# LANGUAGE DeriveDataTypeable
            ,ExistentialQuantification
            ,RankNTypes #-}

-- Define the Controller spec, as well as the basic null controller
module Jaek.UI.Controllers.Base (
  Controller (..)
 ,ControlSet
 -- * Functions
 -- ** ControlSet functions
 ,addController
 ,eFocChangeSet
 ,diagChangeSet
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
import Data.Data()

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
  ,bDiagChange :: Behavior (Diagram Cairo R2 -> Diagram Cairo R2)
  ,eViewChange :: Event (ViewMap -> ViewMap)
  ,response    :: Event (IO ())
  }
  deriving (Typeable)

data EControl = forall st. EControl (Controller st)

type ControlSet = [EControl]

extract :: (forall st. Controller st -> b) -> EControl -> b
extract fn (EControl ctrl) = fn ctrl

addController :: Controller st -> ControlSet -> ControlSet
addController ctrl cset = EControl ctrl : cset

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
             (pure id)
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
filterControlSet cset clicks releases keys motions =
  foldr fn (clicks, releases, keys, motions) cset
 where
  fn ectr (c, r, k, m) = extract (\ctrl -> filterController ctrl c r k m) ectr

eFocChangeSet :: ControlSet -> Event Focus
eFocChangeSet = mconcat . map (extract eFocChange)

-- | Get a behavior of all Diagram modifier functions from the
-- ControlSet, with the first Controllers applied earliest.
diagChangeSet :: ControlSet -> Behavior (Diagram Cairo R2 -> Diagram Cairo R2)
diagChangeSet = foldr f (pure id)
 where
  f ectr endb = (.) <$> modf ectr <*> endb
  modf ectr = (\isActive changeF -> if isActive then changeF else id)
              <$> value (extract dActive ectr)
              <*> extract bDiagChange ectr
