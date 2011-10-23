{-# LANGUAGE DeriveDataTypeable
            ,ExistentialQuantification
            ,RankNTypes #-}

-- Define the Controller spec, as well as the basic null controller
module Jaek.UI.Controllers.Base (
  Controller (..)
 ,ControlSet
 ,ControlGraph
 -- * Functions
 -- ** ControlGraph
 ,buildControlSet
 ,buildController
 ,mkController
 ,addController
 ,bindController
 ,aKeys
 ,aClicks
 ,aMotions
 ,aReleases
 ,cKeys
 ,cClicks
 ,cMotions
 ,cReleases
 ,watch
 -- ** ControlSet functions
 ,diagChangeSet
 ,eFocChangeSet
 ,redrawSet
 ,responseSet
 ,viewChangeSet
 ,zipChangeSet
 -- ** individual controller functions
 ,nullController
 -- ** other functions
 ,passFilter
)

where

import           Graphics.UI.Gtk
import           Jaek.Tree
import           Jaek.UI.FrpTypes
import           Jaek.UI.Focus
import           Jaek.UI.Views

import           Diagrams.Prelude hiding (value)
import           Diagrams.Backend.Cairo
import           Reactive.Banana

import           Data.Data()
import qualified Control.Monad.State as St

-- | used for creating pass-through events.  Typical usage would be
-- 
-- > passFilter clicks isActive passclicks
--   --         nonActive src  ||| active src
-- 
passFilter :: Event a -> Discrete Bool -> Event a -> Event a
passFilter full d e =
  filterApply    (const       <$> value d) e
  <> filterApply (const . not <$> value d) full

-- | A controller reacts to inputs and produces some sort of output.
--   The filters pass along any values to which a controller doesn't respond.
data Controller st = Controller {
   dActive     :: Discrete Bool
  ,dState      :: Discrete st
  ,clickPass   :: Event ClickEvent
  ,releasePass :: Event ClickEvent
  ,keysPass    :: Event KeyVal
  ,motionsPass :: Event MotionEvent
  ,eFocChange  :: Event Focus
  ,eZipChange  :: Event (TreeZip -> TreeZip)
  ,bDiagChange :: Behavior (Diagram Cairo R2 -> Diagram Cairo R2)
  ,eViewChange :: Event (ViewMap -> ViewMap)
  ,responseE    :: Event (IO ())
  ,redrawTrig  :: Event ()
  }
  deriving (Typeable)

data EControl = forall st. EControl (Controller st)

type ControlSet = [EControl]

extract :: (forall st. Controller st -> b) -> EControl -> b
extract fn (EControl ctrl) = fn ctrl

addController' :: Controller st -> ControlSet -> ControlSet
addController' ctrl cset = EControl ctrl : cset

-- | A controller which doesn't do anything.
nullController :: Controller a
nullController =
  Controller (pure False)
             (pure undefined)
             never
             never
             never
             never
             never
             never
             (pure id)
             never
             never
             never

eFocChangeSet :: ControlSet -> Event Focus
eFocChangeSet = mconcat . map (extract eFocChange)

zipChangeSet :: ControlSet -> Event (TreeZip -> TreeZip)
zipChangeSet = mconcat . map (extract eZipChange)

viewChangeSet :: ControlSet -> Event (ViewMap -> ViewMap)
viewChangeSet = mconcat . map (extract eViewChange)

-- | Get a behavior of all Diagram modifier functions from the
-- ControlSet, with the first Controllers applied earliest.
diagChangeSet :: ControlSet -> Behavior (Diagram Cairo R2 -> Diagram Cairo R2)
diagChangeSet = foldr f (pure id)
 where
  f ectr endb = (.) <$> modf ectr <*> endb
  modf ectr = (\isActive changeF -> if isActive then changeF else id)
              <$> value (extract dActive ectr)
              <*> extract bDiagChange ectr

redrawSet :: ControlSet -> Event ()
redrawSet = mconcat . map (extract redrawTrig)

responseSet :: ControlSet -> Event (IO ())
responseSet = mconcat . map (extract responseE)

-- --------------------------------------
-- support for building a ControlSet graph

type ControlGraph a = St.State ControlSet a

buildControlSet
  :: Event ClickEvent      -- ^ clicks
  -> Event ClickEvent      -- ^ releases
  -> Event KeyVal          -- ^ keys
  -> Event MotionEvent     -- ^ motions
  -> ControlGraph a
  -> ControlSet
buildControlSet clicks releases keys motions graph =
  St.execState graph $ addController' ctrl []
 where
  ctrl :: Controller ()
  ctrl = nullController
         { clickPass   = clicks
          ,releasePass = releases
          ,keysPass    = keys
          ,motionsPass = motions
         }

-- | Create a controller from events unhandled by the current ControlSet.
mkController
  :: (Event ClickEvent
      -> Event ClickEvent
      -> Event KeyVal
      -> Event MotionEvent
      -> Controller a)
  -> ControlGraph (Controller a)
mkController f = f <$> cClicks <*> cReleases <*> cKeys <*> cMotions

-- | Add a controller to the current ControlSet
addController :: Controller a -> ControlGraph (Controller a)
addController ctrl = ctrl <$ St.modify (addController' ctrl)

-- | Create and add a controller, combination of mkController and addController
buildController
  :: (Event ClickEvent
      -> Event ClickEvent
      -> Event KeyVal
      -> Event MotionEvent
      -> Controller a)
  -> ControlGraph (Controller a)
buildController f = mkController f >>= addController

-- | create a new controller which takes inputs from pass-through events
-- of the specified controller.
bindController
  :: (Event ClickEvent
      -> Event ClickEvent
      -> Event KeyVal
      -> Event MotionEvent
      -> Controller a)
  -> Controller b
  -> Controller a
bindController f b =
  f (clickPass b) (releasePass b) (keysPass b) (motionsPass b)

-- | clicks passed by the most recently added Controller
cClicks :: ControlGraph (Event ClickEvent)
cClicks = (extract clickPass . head) <$> St.get

cReleases :: ControlGraph (Event ClickEvent)
cReleases = (extract releasePass . head) <$> St.get

cKeys :: ControlGraph (Event KeyVal)
cKeys = (extract keysPass . head) <$> St.get

cMotions :: ControlGraph (Event MotionEvent)
cMotions = (extract motionsPass . head) <$> St.get

-- | all clicks
aClicks :: ControlGraph (Event ClickEvent)
aClicks = (extract clickPass . last) <$> St.get

aReleases :: ControlGraph (Event ClickEvent)
aReleases = (extract releasePass . last) <$> St.get

aKeys :: ControlGraph (Event KeyVal)
aKeys = (extract keysPass . last) <$> St.get

aMotions :: ControlGraph (Event MotionEvent)
aMotions = (extract motionsPass . last) <$> St.get

-- | A useful function for debugging: watch event streams within a ControlGraph
watch :: Show b => String -> Controller a -> (Controller a -> Event b) -> ControlGraph ()
watch str ctrl extractor = do
  let outStream = (\b -> putStrLn $ str ++ " :: " ++ show b) <$> extractor ctrl
  buildController (\cp rp kp mp -> nullController { dActive = pure True
                                                   ,dState  = pure ()
                                                   ,clickPass   = cp
                                                   ,releasePass = rp
                                                   ,keysPass    = kp
                                                   ,motionsPass = mp
                                                   ,responseE   = outStream } )
  return ()
