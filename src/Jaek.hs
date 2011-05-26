{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Graphics.UI.Gtk

import Jaek.Tree
import Jaek.Gen
import Jaek.Render.Tree
import Jaek.StreamExpr
import Jaek.UI
import Diagrams.Backend.Cairo.CmdLine

import Data.Maybe
import qualified Control.Concurrent as Conc

defTree :: TreeZip
defTree = fromMaybe z1 (mkCut [0] 7 10 <$> up z1)
 where
  z1 = mkCut [0] 0 1 $ mkCut [0] 4 2
       $ newSource "Source1" [GenSource Null 10] initialZipper

-- main = defaultMain $ drawTree $ fromZipper defTree
main :: IO ()
main = do
  ignore unsafeInitGUIForThreadedRTS
  ignore $ timeoutAddFull (Conc.yield >> return True) priorityDefaultIdle 50
  win <- createMainWindow
  widgetShowAll win

  ignore $ onDestroy win mainQuit
  mainGUI
