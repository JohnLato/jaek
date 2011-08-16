module Jaek.UI.Controllers.Nav (
 keynavActions
)

where

import Graphics.UI.Gtk
import Jaek.Tree
import Jaek.UI.Focus
import Jaek.UI.FrpHandlers
import Jaek.UI.Controllers.Base

import Reactive.Banana

keynavActions ::
  Behavior Focus
  -> Discrete TreeZip
  -> Event KeyVal
  -> Controller ()
keynavActions bFoc bZip eKey =
  Controller (pure True)
             (pure ())
             defaultPred
             defaultPred
             keyPred
             defaultPred
             eFocChange
             never
             (pure id)
             never
             never
 where
  keyPred _ kv = maybe True (const False) $ keynav undefined undefined kv
  eFocChange = filterMaybes $ (keynav <$> bFoc <*> value bZip) <@> eKey

keynav :: Focus -> TreeZip -> KeyVal -> Maybe Focus
keynav _foc _tz keyval
  | keyval == 65307 = Just Nothing
  | otherwise       = Nothing

