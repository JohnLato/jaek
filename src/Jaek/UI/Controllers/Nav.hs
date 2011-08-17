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
  Behavior Focus
  -> Discrete TreeZip
  -> Event KeyVal
  -> Controller ()
waveNav bFoc bZip eKey =
  nullController { dActive = pure True
                  ,keysPred = keyPred
                  ,eFocChange = focChange }
 where
  keyPred _ kv = maybe True (const False) $ keynav undefined undefined kv
  focChange = filterMaybes $ (keynav <$> bFoc <*> value bZip) <@> eKey

keynav :: Focus -> TreeZip -> KeyVal -> Maybe Focus
keynav _foc _tz keyval
  | keyval == 65307 = Just Nothing
  | otherwise       = Nothing

