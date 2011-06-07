-- | create handlers for menu actions.
module Jaek.UI.MenuActionHandlers (
  createHandlers
 ,newHandler
 ,importHandler
 ,module Jaek.UI.FrpHandlers
)

where

import Graphics.UI.Gtk
import Jaek.IO
import Jaek.StreamExpr
import Jaek.UI.Actions
import Jaek.UI.Dialogs
import Jaek.UI.FrpHandlers

import Reactive.Banana

createHandlers :: ActionGroup -> Window -> IO ()
createHandlers actGrp win =
  mapM_ (\(act,fn) -> do
          z <- act
          fn win z
          actionGroupAddActionWithAccel actGrp z Nothing
        )
        [(quitAction, quitHandler)]

-- ------------------------------------
-- imperative-style handlers

quitHandler :: Window -> Action -> IO (ConnectId Action)
quitHandler win act = on act actionActivated $ widgetDestroy win

-- ------------------------------------
-- FRP handlers
-- these create FRP Events rather than doing basic stuff.

maybeEvent0 :: Typeable a => Action -> IO (Maybe a) -> Prepare (Event a)
maybeEvent0 act ops = fromAddHandler $ \k -> do
   on act actionActivated $ ops >>= maybe (return ()) k
   return ()

-- create a new document
newHandler :: ActionGroup -> Window -> Prepare (Event String)
newHandler actGrp _win = do
  act <- liftIO newAction
  liftIO $ actionGroupAddActionWithAccel actGrp act Nothing
  maybeEvent0 act newProjectDialog

-- import a new audio source
importHandler :: ActionGroup -> Window -> Prepare (Event (String, [StreamExpr]))
importHandler actGrp _win = do
  act <- liftIO importAction
  liftIO $ actionGroupAddActionWithAccel actGrp act Nothing
  maybeEvent0 act $ do
    fc <- fileChooserDialogNew (Just "Select an audio file to import")
             Nothing
             FileChooserActionOpen
             []
    fileChooserSetSelectMultiple fc True
    dialogAddButton fc stockCancel ResponseCancel
    dialogAddButton fc stockOk     ResponseOk
    widgetShowAll fc
    resp <- dialogRun fc
    case resp of
      ResponseOk -> do
        mfp <- fileChooserGetFilename fc
        widgetDestroy fc
        case mfp of
          Nothing -> return Nothing
          Just fp -> do
            eExprs <- newFileExpr fp
            case eExprs of
              Left err    -> print err >> return Nothing
              Right exprs -> return $ Just (fp, exprs)
      _ -> widgetDestroy fc >> return Nothing
