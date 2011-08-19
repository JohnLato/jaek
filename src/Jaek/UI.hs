module Jaek.UI (
  createMainWindow
 ,runInitialMenu
)

where

import           Graphics.UI.Gtk

import           Jaek.Base
import           Jaek.Peaks (defaultPathMap)
import           Jaek.Project
import           Jaek.Tree (HTree)
import           Jaek.UI.Actions
import           Jaek.UI.Controllers
import           Jaek.UI.Dialogs
import           Jaek.UI.Focus
import           Jaek.UI.FrpHandlersCustom
import           Jaek.UI.MenuActionHandlers
import           Jaek.UI.Render

import           Reactive.Banana as FRP
import           Diagrams.Prelude hiding (apply)
import           Data.Tuple.Select

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
  mpRef    <- defaultPathMap

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
    eKeys       <- keypressEvents win
    clicks      <- clickEvents mainArea
    bSz         <- genDSize mainArea
    eRelease    <- releaseEvents mainArea
    motions     <- motionEvents mainArea
    let (drags, _ndReleases) = dragEvents (clicks <> eRelease)
        bFocus  = value dFocus
        eFocus  = changes dFocus
        bFiltInWave = const . isWave <$> bFocus
        selCtrl  = selectCtrl bSz dFocus bZip clicks
                              eRelease drags motions eKeys
        bFName = stepper iProject (fst <$> (eNewDoc <> eOpenDoc))
        (bRoot, _bProjName) = (takeDirectory <$> bFName,
                               takeFileName  <$> bFName)
        (bZip, bView) = genBZip iTree (eNewDoc <> eOpenDoc) eNewSource
                          treeMods eFocus
        bDraw  = genBDraw mpRef bRoot (value bZip) bFocus (value bSz) (value bView)
        dFocus = genDFocus bDraw clicks
                           (eFocChangeSet ctrlSet2)
                           $ ((\tz tmf -> tmf tz) <$> value bZip) <@> treeMods
        -- need to keep several layers of control sets so that events can
        -- be propagated through them.
        ctrlSet1 = addController selCtrl []
        edCtrl1  = keyActions bSz bView selCtrl $ filterApply bFiltInWave eKeys
        filtKeys = sel3 $ filterController edCtrl1 clicks eRelease eKeys motions
        ctrlSet2 = addController (waveNav bFocus bZip filtKeys) $
                   addController edCtrl1 ctrlSet1
        treeMods = zipChangeSet ctrlSet2

    reactimate $ (drawOnExpose mainArea drawRef <$> bDraw <*>
                    value bView <*> bFocus <*> diagChangeSet ctrlSet2)
                 <@> eMainExpose

    -- redraw the window when the state is updated by dirtying the widget.
    let redrawF = widgetQueueDraw mainArea
    reactimate $ ( redrawF <$ eNewDoc)
              <> ( redrawF <$ eOpenDoc)
              <> ( redrawF <$ eNewSource)
              <> ( redrawF <$ eFocus)
              <> ( redrawF <$ motions)

    -- save the document when requested
    reactimate $ apply ((\fp tz () -> writeProject fp tz) <$> bFName <*> value bZip)
                       eSaveDoc

  FRP.actuate network
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

