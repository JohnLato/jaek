module Jaek.UI.Actions (
  menuActions
 ,defActions
 ,fileAction
 ,newAction
 ,openAction
 ,importAction
 ,propsAction
 ,quitAction
 ,renderAction
)

where

import Graphics.UI.Gtk

defActions :: [IO Action]
defActions = [fileAction]

menuActions :: [IO Action]
menuActions = [newAction, quitAction, propsAction]

fileAction :: IO Action
fileAction = actionNew "FileAction" "File" Nothing Nothing

openAction :: IO Action
openAction = actionNew "OpenAction" "Open" Nothing Nothing

newAction :: IO Action
newAction = actionNew "NewAction" "New" Nothing Nothing

importAction :: IO Action
importAction = actionNew "ImportAction" "Import" Nothing Nothing

quitAction :: IO Action
quitAction = actionNew "QuitAction" "Quit" Nothing Nothing

propsAction :: IO Action
propsAction = actionNew "PropsAction" "Properties" Nothing Nothing

renderAction :: IO Action
renderAction = actionNew "RenderAction" "Render" Nothing Nothing
