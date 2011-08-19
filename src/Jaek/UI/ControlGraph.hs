module Jaek.UI.ControlGraph (
  jaekControlGraph
)

where

import Jaek.Tree
import Jaek.UI.Controllers
import Jaek.UI.Focus
import Jaek.UI.FrpTypes
import Jaek.UI.Views

import Reactive.Banana

jaekControlGraph
  :: Discrete (Int,Int)
  -> Discrete Focus
  -> Discrete ViewMap
  -> Discrete TreeZip
  -> Event DragEvent
  -> ControlGraph ()
jaekControlGraph dSize dFocus dView dZip drags = do
  wvSelectCtrl <- buildController (selectCtrl dSize dFocus dZip drags)
  editCtrl     <- addController
                  $ bindController (editCtrl1  dSize dView wvSelectCtrl)
                                   wvSelectCtrl
  buildController (waveNav dFocus dZip)
  return ()
