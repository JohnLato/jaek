module Jaek.UI.Controllers.Nav (
 waveNav
)

where

import Graphics.UI.Gtk
import Jaek.Tree
import Jaek.UI.Focus
import Jaek.UI.FrpHandlers
import Jaek.UI.Controllers.Base

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
waveNav dFoc dZip clicks releases keys motions =
  nullController { dActive     = isActive
                  ,dState      = pure ()
                  ,clickPass   = clicks
                  ,releasePass = releases
                  ,keysPass    = passFilter keys isActive passkeys
                  ,motionsPass = motions
                  ,eFocChange = focChange }
 where
  isActive = pure True
  (passkeys, focChange) = splitEithers $ (keynav <$> dFoc <*> dZip)
                                         <@> keys

keynav :: Focus -> TreeZip -> KeyVal -> Either KeyVal Focus
keynav _foc _tz keyval
  | keyval == 65307 = Right Nothing
  | otherwise       = Left keyval

