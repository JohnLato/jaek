module Jaek.UI.FrpHandlers (
  exposeEvents
)

where

import Graphics.UI.Gtk
import Jaek.Base
import Reactive.Banana

exposeEvents widget =
  fromAddHandler $ \k -> ignore $ widget `onExpose` const (k () >> return False)

