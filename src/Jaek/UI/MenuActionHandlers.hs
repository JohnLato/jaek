{-# LANGUAGE TupleSections #-}

-- | create handlers for menu actions.
module Jaek.UI.MenuActionHandlers (
  createHandlers
 ,newHandler
 ,openHandler
 ,saveHandler
 ,importHandler
 ,deleteHandler
 ,muteHandler
 ,module Jaek.UI.FrpHandlers
)

where

import Graphics.UI.Gtk
import Jaek.IO
import Jaek.StreamExpr
import Jaek.Tree
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

defaultMkAction
  :: IO Action
  -> ActionGroup
  -> Window
  -> NetworkDescription (Event ())
defaultMkAction ioact actGrp _win = do
  act <- liftIO ioact
  liftIO $ actionGroupAddActionWithAccel actGrp act Nothing
  maybeEvent0 act $ return $ Just ()

-- ------------------------------------
-- imperative-style handlers

quitHandler :: Window -> Action -> IO (ConnectId Action)
quitHandler win act = on act actionActivated $ widgetDestroy win

-- ------------------------------------
-- FRP handlers
-- these create FRP Events rather than doing basic stuff.

maybeEvent0 :: Typeable a => Action -> IO (Maybe a) -> NetworkDescription (Event a)
maybeEvent0 act ops = do
   (addHandler, runHandlers) <- liftIO newAddHandler
   liftIO $ on act actionActivated $ ops >>= maybe (return ()) runHandlers
   fromAddHandler addHandler

-- |create a new document
newHandler :: ActionGroup -> Window -> NetworkDescription (Event (String, HTree))
newHandler actGrp _win = do
  act <- liftIO newAction
  liftIO $ actionGroupAddActionWithAccel actGrp act Nothing
  maybeEvent0 act newProjectDialog

-- | Load a saved project file
openHandler :: ActionGroup -> Window -> NetworkDescription (Event (String, HTree))
openHandler actGrp _win = do
  act <- liftIO openAction
  liftIO $ actionGroupAddActionWithAccel actGrp act Nothing
  maybeEvent0 act openProjectDialog

-- | Save the current project file
saveHandler :: ActionGroup -> Window -> NetworkDescription (Event ())
saveHandler actGrp _win = do
  act <- liftIO saveAction
  liftIO $ actionGroupAddActionWithAccel actGrp act Nothing
  maybeEvent0 act $ return $ Just ()

-- |import a new audio source
importHandler :: ActionGroup -> Window -> NetworkDescription (Event (String, [StreamExpr]))
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


-- -----------------------------------------
-- edit event sources


-- | delete action
deleteHandler :: ActionGroup -> Window -> NetworkDescription (Event ())
deleteHandler = defaultMkAction deleteAction

-- | mute action
muteHandler :: ActionGroup -> Window -> NetworkDescription (Event ())
muteHandler = defaultMkAction muteAction
