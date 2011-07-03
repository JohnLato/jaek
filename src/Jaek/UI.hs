module Jaek.UI (
  createMainWindow
 ,runInitialMenu
)

where

import           Graphics.UI.Gtk

import           Jaek.Base
import           Jaek.Project
import           Jaek.Tree (HTree)
import           Jaek.UI.Actions
import           Jaek.UI.Dialogs
import           Jaek.UI.Focus
import           Jaek.UI.FrpHandlersCustom
import           Jaek.UI.Input.Actions
import           Jaek.UI.Input.Drags
import           Jaek.UI.MenuActionHandlers
import           Jaek.UI.Render

import           Reactive.Banana as FRP
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
  drawRef  <- initDrawRef mainArea

  -- add FRP handler stuff...
  widgetAddEvents mainArea
    [KeyPressMask, ButtonPressMask, ButtonReleaseMask, ButtonMotionMask]

  network <- FRP.compile $ do
    eNewDoc     <- newHandler standardGroup win
    eOpenDoc    <- openHandler standardGroup win
    eSaveDoc    <- saveHandler standardGroup win
    eNewSource  <- importHandler standardGroup win
    eMainExpose <- exposeEvents mainArea
    -- only the window receives keypress events...
    eKeypresses <- keypressEvents win
    clicks      <- clickEvents mainArea
    bSz         <- genBSize mainArea
    eRelease    <- releaseEvents mainArea
    motions     <- motionEvents mainArea
    let (drags, _ndReleases) = dragEvents (clicks <> eRelease)
        bFiltInWave = const . isWave <$> bFocus
        treeMods = keyActions bSz bView bSels $
                     filterApply bFiltInWave eKeypresses
        bSels  = bSelection bSz bFocus bZip clicks eRelease drags motions
        bFName = stepper iProject (fst <$> (eNewDoc <> eOpenDoc))
        (bRoot, _bProjName) = (takeDirectory <$> bFName,
                               takeFileName  <$> bFName)
        (bZip, bView) = genBZip iTree (eNewDoc <> eOpenDoc) eNewSource treeMods
        bDraw = genBDraw bRoot bZip bFocus bSz bView
        (bFocus, eFocChange) = genBFocus bDraw clicks $ apply
                                ((\tz tmf -> tmf tz) <$> bZip) treeMods
    reactimate $ apply (drawOnExpose mainArea drawRef <$> bDraw
                          <*> bView <*> bFocus <*> bSels)
                       eMainExpose

    -- redraw the window when the state is updated by dirtying the widget.
    let redrawF = widgetQueueDraw mainArea
    reactimate $ ( redrawF <$ eNewDoc)
              <> ( redrawF <$ eOpenDoc)
              <> ( redrawF <$ eNewSource)
              <> ( redrawF <$ eFocChange)
              <> ( redrawF <$ motions)

    -- save the document when requested
    reactimate $ apply ((\fp tz () -> writeProject fp tz) <$> bFName <*> bZip)
                       eSaveDoc

  FRP.run network
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

