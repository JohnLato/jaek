module Jaek.UI (
  createMainWindow
 ,runInitialMenu
)

where

import           Graphics.UI.Gtk

import           Jaek.Base
import           Jaek.IO
import           Jaek.Peaks (defaultPathMap)
import           Jaek.Project
import           Jaek.Tree (HTree, liftZ, getExprs)
import           Jaek.UI.Actions
import           Jaek.UI.AllSources
import           Jaek.UI.ControlGraph
import           Jaek.UI.Controllers
import           Jaek.UI.Dialogs
import           Jaek.UI.FrpHandlersCustom
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
  \      <menuitem name=\"Render\" action=\"RenderAction\" />\
  \      <menuitem name=\"Close\" action=\"QuitAction\" />\
  \    </menu>\
  \    <menu name=\"Edit\" action=\"EditAction\">\
  \      <menuitem name=\"Mute\" action=\"MuteAction\" />\
  \      <menuitem name=\"Delete\" action=\"DeleteAction\" />\
  \    </menu>\
  \    <menu name=\"View\" action=\"ViewAction\">\
  \      <menuitem name=\"Zoom In\" action=\"ZoomInAction\" />\
  \      <menuitem name=\"Zoom Out\" action=\"ZoomOutAction\" />\
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
    evtSrcs     <- makeSources standardGroup win
    let eNewDoc  = getNewSource evtSrcs
        eOpenDoc = getOpenSource evtSrcs
        eSaveDoc = getSaveSource evtSrcs
        eNewSource = getImportSource evtSrcs
        eRender    = getRenderSource evtSrcs

    eMainExpose <- exposeEvents mainArea
    -- only the window receives keypress events...
    eKeys       <- keypressEvents win
    clicks      <- clickEvents mainArea
    bSz         <- genDSize mainArea
    eRelease    <- releaseEvents mainArea
    motions     <- motionEvents mainArea
    let bFocus  = FRP.value dFocus
        eFocus  = changes dFocus
        bFName = stepper iProject (fst <$> (eNewDoc <> eOpenDoc))
        (bRoot, _bProjName) = (takeDirectory <$> bFName,
                               takeFileName  <$> bFName)
        (bZip, bView) = genBZip iTree (eNewDoc <> eOpenDoc) eNewSource
                          treeMods (viewChangeSet ctrlSet) eFocus
        bDraw  = genBDraw mpRef bRoot (FRP.value bZip) bFocus
                          (FRP.value bSz) (FRP.value bView)
        dFocus = genDFocus bDraw clicks
                           (eFocChangeSet ctrlSet)
                           $ ((\tz tmf -> tmf tz) <$> FRP.value bZip)
                             <@> treeMods
        ctrlSet  = buildControlSet clicks eRelease eKeys motions $
                     jaekControlGraph evtSrcs bSz dFocus bView bZip
        treeMods = zipChangeSet ctrlSet

    reactimate $ (drawOnExpose mainArea drawRef <$> bDraw <*>
                    FRP.value bView <*> bFocus <*> diagChangeSet ctrlSet)
                 <@> eMainExpose

    -- add in any effects from the ControlSet
    reactimate $ responseSet ctrlSet

    -- redraw the window when the state is updated by dirtying the widget.
    let redrawF = widgetQueueDraw mainArea
    reactimate $ ( redrawF <$ eNewDoc)
              <> ( redrawF <$ eOpenDoc)
              <> ( redrawF <$ eNewSource)
              <> ( redrawF <$ eFocus)
              <> ( redrawF <$ redrawSet ctrlSet)

    -- save the document when requested
    reactimate $ apply ((\fp tz () -> writeProject fp tz)
                          <$> bFName <*> FRP.value bZip)
                       eSaveDoc
    -- render output when requested
    reactimate $ (\tz fmt -> renderExprs fmt (liftZ getExprs tz))
                     <$> FRP.value bZip
                 <@> eRender

  FRP.actuate network
  ui <- uiManagerNew
  ignore $ uiManagerAddUiFromString ui uiDef
  uiManagerInsertActionGroup ui standardGroup 0
  uiManagerGetAccelGroup ui >>= windowAddAccelGroup win

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

