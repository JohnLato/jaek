module Jaek.UI.Controllers.Nav (
  waveNav
 ,allNav
)

where

import Graphics.UI.Gtk
import Jaek.Tree
import Jaek.UI.Controllers.Base
import Jaek.UI.Focus
import Jaek.UI.FrpHandlers
import Jaek.UI.Views

import Reactive.Banana

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
  Discrete Focus
  -> Discrete TreeZip
  -> Discrete ViewMap
  -> Event ClickEvent
  -> Event ClickEvent
  -> Event KeyVal
  -> Event MotionEvent
  -> Controller ()
allNav dFocus dZip dVmap clicks releases keys motions =
  nullController { dActive     = isActive
                  ,dState      = pure ()
                  ,clickPass   = clicks
                  ,releasePass = releases
                  ,keysPass    = passkeys
                  ,motionsPass = motions
                  ,eViewChange = vmapChange
                  ,redrawTrig  = () <$ vmapChange }
 where
  isActive = pure True
  dView = (\vm z foc -> getView vm . hole $ goToFocus z foc)
          <$> dVmap <*> dZip <*> dFocus
  (passkeys, vmapChange) = splitEithers $ (treeKey <$> dView <*> dZip) <@> keys

treeKey :: View -> TreeZip -> KeyVal -> Either KeyVal (ViewMap -> ViewMap)
treeKey v@(FullView {}) tz keyval = case keyval of
  65361 -> Right (uf $ slideX (-0.40) v)  -- left
  65362 -> Right (uf $ slideY (0.40) v)  -- up
  65363 -> Right (uf $ slideX (0.40) v)  -- right
  65364 -> Right (uf $ slideY (-0.40) v)  -- down
  _     -> Left keyval
 where
  -- always set zipper to the root node path, just for updating the map
  uf = updateMap (goToHead tz) . ModView
treeKey v@(WaveView {}) tz keyval = case keyval of
  65361 -> Right (uf $ slideX (-0.25) v)  -- left
  65363 -> Right (uf $ slideX (0.25) v)   -- right
  _     -> Left keyval
 where
  -- always set zipper to the root node path, just for updating the map
  uf = updateMap tz . ModView
