module Jaek.UI (
  createMainWindow
)

where

import           Graphics.UI.Gtk

import           Jaek.Base
import           Jaek.Gen
import           Jaek.Render.Tree
import           Jaek.StreamExpr
import           Jaek.Tree
import           Jaek.UI.Actions
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

createMainWindow :: IO Window
createMainWindow = do
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
    let bRoot = accumB "" ((\nm _ -> nm) <$> eNewDoc)  -- name of root directory
    let bZip  = accumB initialZipper (
                 ((\_ -> const initialZipper) <$> eNewDoc)
                 <>
                 (uncurry newSource <$> eNewSource))
    let bDraw = (toGtkCoords . scale 150 . drawTree . fromZipper) <$> bZip
    reactimate $ apply ((\d _ -> widgetGetDrawWindow mainArea >>= flip renderToGtk d) <$> bDraw) eMainExpose
    -- redraw the window when the state is updated...
    reactimate $ const (widgetQueueDraw mainArea) <$>
                 eNewDoc <> fmap fst eNewSource

    let testClickE = apply ((\d clk ->  print $ runQuery (query d)
                           (P (xPos clk, yPos clk)) ) <$> bDraw)
                           clicks
    reactimate $ (print <$> clicks) <> testClickE

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

defTree :: TreeZip
defTree = fromMaybe z1 (mkCut [0] 7 10 <$> up z1)
 where
  z1 = mkCut [0] 0 1 $ mkCut [0] 4 2
       $ newSource "Source1" [GenSource Null 10] initialZipper

