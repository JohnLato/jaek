{-# LANGUAGE CPP #-}

module Jaek.UI.ControlGraph (
  jaekControlGraph
)

where

import Jaek.Tree
import Jaek.UI.AllSources
import Jaek.UI.Controllers
import Jaek.UI.Focus
import Jaek.UI.Views

import Reactive.Banana
import Control.Monad (when)

jaekControlGraph
  :: Sources
  -> Discrete (Int,Int)
  -> Discrete Focus
  -> Discrete ViewMap
  -> Discrete TreeZip
  -> ControlGraph ()
jaekControlGraph sources dSize dFocus dViewMap dZip = do
  baseNav      <- buildController (allNav sources dFocus dZip dViewMap)
  let dView = dState baseNav
  wvSelectCtrl <- buildController (selectCtrl dSize dView dZip)
  clipCtrl     <- buildController
                    (clipboardCtrl dSize dViewMap dZip wvSelectCtrl sources)
  let editCtrl = bindController (editCtrl1 dSize dViewMap clipCtrl
                                    wvSelectCtrl sources)
                                wvSelectCtrl
  addController editCtrl

  when DEBUG $ do
    watch "waveSelection" wvSelectCtrl (changes . dState)
    watch "clipboard" clipCtrl (changes . dState)
    watch "commands" editCtrl
      ( fmap (liftZ getTransforms) . applyD (flip ($) <$> dZip) . eZipChange )

  buildController (waveNav dFocus dZip)

  return ()
