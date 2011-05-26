module Jaek.UI.Dialogs (
  warnOnException
)

where

import Graphics.UI.Gtk
import Jaek.Base

import Control.Exception

warnOnException :: Exception e => e -> IO ()
warnOnException e = do
  dlg <- messageDialogNew Nothing
                          [DialogModal]
                          MessageWarning
                          ButtonsClose
                          (show e)
  ignore $ dialogRun dlg
  widgetDestroy dlg

