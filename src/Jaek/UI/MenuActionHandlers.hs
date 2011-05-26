-- | create handlers for menu actions.
module Jaek.UI.MenuActionHandlers (
  createHandlers
 ,newHandler
)

where

import Graphics.UI.Gtk
import Jaek.Base
import Jaek.UI.Actions
import Jaek.UI.Dialogs

import Reactive.Banana
import System.Directory
import Control.Exception
import Control.Monad.Trans.Maybe

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

-- | on actionActivated, perform the @IO a@ and create an @Event a@ from
-- the result.
event0 :: Typeable a => Action -> IO a -> Prepare (Event a)
event0 act ops = fromAddHandler $ \k -> do
  on act actionActivated $ ops >>= k
  return ()

maybeEvent0 :: Typeable a => Action -> IO (Maybe a) -> Prepare (Event a)
maybeEvent0 act ops = fromAddHandler $ \k -> do
   on act actionActivated $ ops >>= maybe (return ()) k
   return ()

-- create a new document
newHandler :: ActionGroup -> Window -> Prepare (Event String)
newHandler actGrp win = do
  act <- liftIO newAction
  liftIO $ actionGroupAddActionWithAccel actGrp act Nothing
  maybeEvent0 act $ do
    fc <- fileChooserDialogNew (Just "Enter a name for this project")
             Nothing
             FileChooserActionCreateFolder
             []
    dialogAddButton fc stockCancel ResponseCancel
    dialogAddButton fc stockOk     ResponseOk
    widgetShowAll fc
    resp <- dialogRun fc
    case resp of
      ResponseOk -> do
        mfp <- try $ runMaybeT $ do
                 fp <- MaybeT $ fileChooserGetFilename fc
                 liftIO $ setCurrentDirectory fp
                 return fp
        widgetDestroy fc
        let handleE :: SomeException -> IO (Maybe String)
            handleE e = warnOnException e >> return Nothing
        either handleE return mfp
      _ -> widgetDestroy fc >> return Nothing

