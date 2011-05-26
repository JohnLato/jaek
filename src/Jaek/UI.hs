module Jaek.UI (
  createMainWindow
)

where

import           Graphics.UI.Gtk

import           Jaek.Base
import           Jaek.UI.Actions
import           Jaek.UI.MenuActionHandlers

import           Reactive.Banana

uiDef :: String
uiDef =
  "<ui>\
  \  <menubar>\
  \    <menu name=\"File\" action=\"FileAction\">\
  \      <menuitem name=\"New\" action=\"NewAction\" />\
  \      <menuitem name=\"Close\" action=\"QuitAction\" />\
  \    </menu>\
  \  </menubar>\
  \</ui>"

createMainWindow :: IO Window
createMainWindow = do
  win <- windowNew

  standardGroup <- actionGroupNew "standard"
  -- add handlers which aren't used (placeholders, spacers, menu names)
  -- and otherwise wouldn't be created.
  mapM_ (>>= actionGroupAddAction standardGroup) defActions

  -- add imperative-style handlers.
  createHandlers standardGroup win

  -- add FRP handler stuff...
  prepareEvents $ do
    eNewDoc <- newHandler standardGroup win
    reactimate $ fmap print eNewDoc
  -- create new widgets

  ui <- uiManagerNew
  ignore $ uiManagerAddUiFromString ui uiDef
  uiManagerInsertActionGroup ui standardGroup 0

  ignore $ onDestroy win mainQuit
  ignore $ onSizeRequest win $ return (Requisition 700 500)
  (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"

  vbox <- vBoxNew False 0
  set vbox [boxHomogeneous := False]
  hb1 <- hBoxNew False 0
  set hb1 [boxHomogeneous := False]

  vb2 <- vBoxNew False 0

  -- main window goes here
  boxPackStart vbox menuBar PackNatural 0
  boxPackStart vbox hb1 PackNatural 0
  boxPackStart hb1 vb2  PackGrow 0

  containerAdd win vbox
  return win

