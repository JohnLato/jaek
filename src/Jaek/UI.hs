module Jaek.UI (
  createMainWindow
 ,runInitialMenu
)

where

import           Graphics.UI.Gtk

import           Jaek.Base
import           Jaek.Project
import           Jaek.Render
import           Jaek.Tree (HTree)
import           Jaek.UI.Actions
import           Jaek.UI.Dialogs
import           Jaek.UI.FrpHandlersCustom
import           Jaek.UI.MenuActionHandlers

import           Reactive.Banana
import           Diagrams.Backend.Cairo.Gtk
import           Diagrams.Prelude hiding (apply)

import           System.FilePath

uiDef :: String
uiDef =
  "<ui>\
  \  <menubar>\
  \    <menu name=\"File\" action=\"FileAction\">\
  \      <menuitem name=\"New\" action=\"NewAction\" />\
  \      <menuitem name=\"Open\" action=\"OpenAction\" />\
  \      <menuitem name=\"Save\" action=\"SaveAction\" />\
  \      <menuitem name=\"Import\" action=\"ImportAction\" />\
  \      <menuitem name=\"Close\" action=\"QuitAction\" />\
  \    </menu>\
  \  </menubar>\
  \</ui>"

createMainWindow :: String -> HTree -> IO Window
createMainWindow iProject iTree = do
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
  widgetAddEvents mainArea
    [ButtonPressMask, ButtonReleaseMask, ButtonMotionMask]

  prepareEvents $ do
    eNewDoc     <- newHandler standardGroup win
    eOpenDoc    <- openHandler standardGroup win
    eSaveDoc    <- saveHandler standardGroup win
    eNewSource  <- importHandler standardGroup win
    eMainExpose <- exposeEvents mainArea
    clicks      <- clickEvents mainArea
    bSize       <- genBSize mainArea
    let bFName = accumB (iProject) (const . fst <$> (eNewDoc <> eOpenDoc))
        (bRoot, bProjName) = (takeDirectory <$> bFName,
                              takeFileName  <$> bFName)
        (bZip, bView) = genBZip iTree (eNewDoc <> eOpenDoc) eNewSource
        bDraw = genBDraw bRoot bZip bFocus bSize bView
        (bFocus, eFocChange) = genBFocus bDraw clicks
    reactimate $ apply ((\d _ -> do
                 dw <- widgetGetDrawWindow mainArea
                 renderToGtk dw d) <$> bDraw) eMainExpose

    -- redraw the window when the state is updated by dirtying the widget.
    let redrawF = widgetQueueDraw mainArea
    reactimate $ ( redrawF <$ eNewDoc)
              <> ( redrawF <$ eOpenDoc)
              <> ( redrawF <$ eNewSource)
              <> ( redrawF <$ eFocChange)

    -- save the document when requested
    reactimate $ apply ((\fp tz () -> writeProject fp tz) <$> bFName <*> bZip)
                       eSaveDoc

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

