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
import           Data.Maybe
import           Data.Monoid

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

  -- create new widgets
  mainArea <- drawingAreaNew

  -- add FRP handler stuff...

  prepareEvents $ do
    eNewDoc <- newHandler standardGroup win
    eMainExpose <- exposeEvents mainArea
    let bZip = accumB initialZipper ( (\nm -> newSource nm []) <$> eNewDoc)
    let bDraw = (const . renderToDrawingArea mainArea . drawTree . fromZipper)
                <$> bZip
    reactimate $ fmap print eNewDoc
    reactimate $ apply bDraw (eMainExpose `mappend` (() <$ eNewDoc))

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

-- main = defaultMain $ drawTree $ fromZipper defTree

