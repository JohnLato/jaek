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

jaekControlGraph
  :: Sources
  -> Discrete (Int,Int)
  -> Discrete Focus
  -> Discrete ViewMap
  -> Discrete TreeZip
  -> ControlGraph ()
jaekControlGraph sources dSize dFocus dView dZip = do
  baseNav      <- buildController (allNav sources dFocus dZip dView)
  wvSelectCtrl <- buildController (selectCtrl dSize dFocus dZip)
  addController $ bindController (editCtrl1  dSize dView wvSelectCtrl sources)
                                 wvSelectCtrl
  buildController (waveNav dFocus dZip)

  return ()
