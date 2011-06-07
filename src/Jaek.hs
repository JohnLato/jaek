{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Graphics.UI.Gtk

import Jaek.Gen
import Jaek.UI

import qualified Control.Concurrent as Conc

main :: IO ()
main = do
  ignore unsafeInitGUIForThreadedRTS
  ignore $ timeoutAddFull (Conc.yield >> return True) priorityDefaultIdle 50

  mProjName <- runInitialMenu
  case mProjName of
    Nothing -> return ()
    Just nm -> do
      win <- createMainWindow nm
      widgetShowAll win

      ignore $ onDestroy win mainQuit
      mainGUI
