module Jaek.UI.Controllers.Nav (
  waveNav
 ,allNav
)

where

import Graphics.UI.Gtk
import Jaek.Tree
import Jaek.UI.AllSources
import Jaek.UI.Controllers.Base
import Jaek.UI.Focus
import Jaek.UI.FrpHandlers
import Jaek.UI.Views

import Reactive.Banana
import Data.Monoid

-- | navigation valid from within a Wave view
waveNav ::
  Discrete Focus
  -> Discrete TreeZip
  -> Event ClickEvent
  -> Event ClickEvent
  -> Event KeyVal
  -> Event MotionEvent
  -> Controller ()
waveNav dFocus dZip clicks releases keys motions =
  nullController { dActive     = isActive
                  ,dState      = pure ()
                  ,clickPass   = clicks
                  ,releasePass = releases
                  ,keysPass    = passFilter keys isActive passkeys
                  ,motionsPass = motions
                  ,eFocChange = focChange }
 where
  isActive = isWave <$> dFocus
  (passkeys, focChange) = splitEithers $ (waveKey <$> dFocus <*> dZip)
                                         <@> keys

waveKey :: Focus -> TreeZip -> KeyVal -> Either KeyVal Focus
waveKey _foc _tz keyval
  | keyval == 65307 = Right treeFocus
  | otherwise       = Left keyval

-- | navigation valid from within both views
allNav ::
  Sources
  -> Discrete Focus
  -> Discrete TreeZip
  -> Discrete ViewMap
  -> Event ClickEvent
  -> Event ClickEvent
  -> Event KeyVal
  -> Event MotionEvent
  -> Controller View
allNav sources dFocus dZip dVmap clicks releases keys motions =
  nullController { dActive     = isActive
                  ,dState      = dView
                  ,clickPass   = clicks
                  ,releasePass = releases
                  ,keysPass    = pass2
                  ,motionsPass = motions
                  ,eViewChange = vmapChange `mappend` zoomChange
                  ,redrawTrig  = (() <$ vmapChange) `mappend` (() <$ zooms) }
 where
  isActive = pure True
  dView = (\vm z foc -> getView vm . hole $ goToFocus z foc)
          <$> dVmap <*> dZip <*> dFocus
  (pass1, vmapChange) = splitEithers $ (treeKey <$> dView <*> dZip) <@> keys
  (pass2, zooms')        = splitEithers $ zoomKey <$> pass1
  zooms                  = mconcat
                            [zooms'
                            ,getZoomInSource sources
                            ,getZoomOutSource sources]
  zoomChange             = (zoomF <$> dView <*> dZip) <@> zooms

zoomKey :: KeyVal -> Either KeyVal Zoom
zoomKey 43 = Right $ Zoom 0.5
zoomKey 95 = Right $ Zoom 2
zoomKey k  = Left k

zoomF :: View -> TreeZip -> Zoom -> ViewMap -> ViewMap
zoomF v@(FullView {}) tz zm = updateMap (goToHead tz) . ModView $ zoom zm v
zoomF v               tz zm = updateMap tz            . ModView $ zoom zm v

treeKey :: View -> TreeZip -> KeyVal -> Either KeyVal (ViewMap -> ViewMap)
treeKey v@(FullView {}) tz keyval = case keyval of
  65361 -> Right (uf $ slideX (-0.40) v)  -- left
  65362 -> Right (uf $ slideY 0.40    v)  -- up
  65363 -> Right (uf $ slideX 0.40    v)  -- right
  65364 -> Right (uf $ slideY (-0.40) v)  -- down
  _     -> Left keyval
 where
  -- always set zipper to the root node path, just for updating the map
  uf = updateMap (goToHead tz) . ModView
treeKey v@(WaveView {}) tz keyval = case keyval of
  65361 -> Right (uf $ slideX (-0.25) v)  -- left
  65363 -> Right (uf $ slideX 0.25    v)   -- right
  _     -> Left keyval
 where
  -- always set zipper to the root node path, just for updating the map
  uf = updateMap tz . ModView

