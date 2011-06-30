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
import           Jaek.UI.FrpHandlersCustom
import           Jaek.UI.MenuActionHandlers
import           Jaek.UI.Render
import           Jaek.UI.Views

import           Reactive.Banana as FRP
import           Diagrams.Prelude hiding (apply)

import           Control.Concurrent.STM
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
    [ButtonPressMask, ButtonReleaseMask, ButtonMotionMask]

  network <- FRP.compile $ do
    eNewDoc     <- newHandler standardGroup win
    eOpenDoc    <- openHandler standardGroup win
    eSaveDoc    <- saveHandler standardGroup win
    eNewSource  <- importHandler standardGroup win
    eMainExpose <- exposeEvents mainArea
    clicks      <- clickEvents mainArea
    bSize       <- genBSize mainArea
    eRelease    <- releaseEvents mainArea
    motions     <- motionEvents mainArea
    let drags = dragEvents (clicks <> eRelease)
        bS1 = bCurSelection drags
                            (() <$ filterE (not . clickIsAdditive) clicks)
        bS2 = genBDrag (clicks <> eRelease) motions
        bSelForDraw = (\xs mx -> maybe xs (\x -> x:xs) mx) <$> bS1 <*> bS2
        bFName = stepper iProject (fst <$> (eNewDoc <> eOpenDoc))
        (bRoot, bProjName) = (takeDirectory <$> bFName,
                              takeFileName  <$> bFName)
        (bZip, bView) = genBZip iTree (eNewDoc <> eOpenDoc) eNewSource
        bDraw = genBDraw bRoot bZip bFocus bSize bView
        (bFocus, eFocChange) = genBFocus bDraw clicks
    reactimate $ apply (drawOnExpose mainArea drawRef <$> bDraw
                          <*> bView <*> bFocus <*> bSelForDraw)
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

