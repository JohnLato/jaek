module Jaek.UI.AllSources (
  Sources (..)
 ,makeSources
)

where

import Graphics.UI.Gtk

import Jaek.StreamExpr
import Jaek.Tree
import Jaek.UI.MenuActionHandlers
import Jaek.UI.Views (Zoom (..))
import Reactive.Banana

data Sources = Sources {
  getNewSource     :: Event (String, HTree)
 ,getOpenSource    :: Event (String, HTree)
 ,getSaveSource    :: Event ()
 ,getImportSource  :: Event (String, [StreamExpr])
 ,getZoomInSource  :: Event Zoom
 ,getZoomOutSource :: Event Zoom
 ,getCopySource    :: Event ()
 ,getDeleteSource  :: Event ()
 ,getInsertSource  :: Event ()
 ,getMuteSource    :: Event ()
 }

makeSources :: ActionGroup -> Window -> NetworkDescription Sources
makeSources actGrp win =
  Sources
  <$> newHandler     actGrp win
  <*> openHandler    actGrp win
  <*> saveHandler    actGrp win
  <*> importHandler  actGrp win
  <*> zoomInHandler  actGrp win
  <*> zoomOutHandler actGrp win
  <*> deleteHandler  actGrp win
  <*> insertHandler  actGrp win
  <*> muteHandler    actGrp win
  <*> copyHandler     actGrp win
