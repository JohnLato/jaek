module Jaek.UI (
  createMainWindow
 ,runInitialMenu
)

where

import           Graphics.UI.Gtk

import           Jaek.Base
import           Jaek.Gen
import           Jaek.Render
import           Jaek.StreamExpr
import           Jaek.Tree
import           Jaek.UI.Actions
import           Jaek.UI.Dialogs
import           Jaek.UI.MenuActionHandlers

import           Reactive.Banana
import           Diagrams.Backend.Cairo.Gtk
import           Diagrams.Prelude hiding (apply)
import           Data.Maybe

uiDef :: String
uiDef =
  "<ui>\
  \  <menubar>\
  \    <menu name=\"File\" action=\"FileAction\">\
  \      <menuitem name=\"New\" action=\"NewAction\" />\
  \      <menuitem name=\"Import\" action=\"ImportAction\" />\
  \      <menuitem name=\"Close\" action=\"QuitAction\" />\
  \    </menu>\
  \  </menubar>\
  \</ui>"

createMainWindow :: String -> IO Window
createMainWindow iProject = do
  win <- windowNew

  standardGroup <- actionGroupNew "standard"
  -- add handlers which aren't used (placeholders, spacers, menu names)
  -- and otherwise wouldn't be created.
  mapM_ (>>= actionGroupAddAction standardGroup) defActions

  -- add imperative-style handlers.
  createHandlers standardGroup win

  -- create new widgets
  mainArea <- drawingAreaNew

  -- add FRP handler stuff...

  prepareEvents $ do
    eNewDoc     <- newHandler standardGroup win
    eNewSource  <- importHandler standardGroup win
    eMainExpose <- exposeEvents mainArea
    clicks      <- clickEvents mainArea
    bSize       <- genBSize mainArea
    let bRoot = accumB iProject ((\nm _ -> nm) <$> eNewDoc)
        bZip  = accumB initialZipper $
                  (const initialZipper <$ eNewDoc)
                  <> (uncurry newSource <$> eNewSource)
        bDraw = genBDraw bZip bFocus ( (\(x,y) -> (fI x, fI y)) <$> bSize)
        (bFocus, eFocChange) = genBFocus bDraw clicks
    reactimate $ apply ((\d _ -> do
                 dw <- widgetGetDrawWindow mainArea
                 renderToGtk dw d) <$> bDraw) eMainExpose

    -- redraw the window when the state is updated by dirtying the widget.
    let redrawF = widgetQueueDraw mainArea
    reactimate $ ( redrawF <$ eNewDoc)
              <> ( redrawF <$ eNewSource)
              <> ( redrawF <$ eFocChange)

  ui <- uiManagerNew
  ignore $ uiManagerAddUiFromString ui uiDef
  uiManagerInsertActionGroup ui standardGroup 0

  ignore $ onDestroy win mainQuit
  ignore $ onSizeRequest win $ return (Requisition 700 500)
  (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"

  vbox <- vBoxNew False 0
  set vbox [boxHomogeneous := False]

  -- main window goes here
  boxPackStart vbox menuBar  PackNatural 0
  boxPackStart vbox mainArea PackGrow 0

  containerAdd win vbox
  return win

