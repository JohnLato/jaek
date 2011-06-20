module Jaek.UI.Dialogs (
  warnOnException
 ,runInitialMenu
 ,newProjectDialog
)

where

import Graphics.UI.Gtk
import Jaek.Base
import Jaek.Project

import Control.Exception
import Control.Monad.Trans.Maybe

import System.Directory

warnOnException :: Exception e => e -> IO ()
warnOnException e = do
  dlg <- messageDialogNew Nothing
                          [DialogModal]
                          MessageWarning
                          ButtonsClose
                          (show e)
  ignore $ dialogRun dlg
  widgetDestroy dlg

initialMenu :: IO (Dialog, RadioButton, RadioButton)
initialMenu = do
  dlg <- dialogNew
  vb  <- dialogGetUpper dlg
  r1  <- radioButtonNewWithMnemonic "New Project"

  -- Option for loading an existing project
  r2  <- radioButtonNewWithMnemonicFromWidget r1 "Open a file"

  boxPackStart vb r1 PackGrow 2
  boxPackStart vb r2 PackGrow 2

  dialogAddButton dlg "Cancel" ResponseCancel
  dialogAddButton dlg "Ok"     ResponseOk
  return (dlg, r1, r2)

runInitialMenu :: IO (Maybe String)
runInitialMenu = do
  (dlg, r1, _r2) <- initialMenu
  widgetShowAll dlg

  rsp <- dialogRun dlg
  case rsp of
    ResponseOk -> toggleButtonGetActive r1 >>= \r1Active -> if r1Active
      then do
        -- create
        widgetDestroy dlg
        newProjectDialog
      else do
        -- load an existing project
        error "Can't load existing projects yet."
    _ -> widgetDestroy dlg >> return Nothing

newProjectDialog :: IO (Maybe String)
newProjectDialog = do
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
               liftIO $ createDirectoryIfMissing False peakDir
               return fp
      widgetDestroy fc
      let handleE :: SomeException -> IO (Maybe String)
          handleE e = warnOnException e >> return Nothing
      either handleE return mfp
    _ -> widgetDestroy fc >> return Nothing
