module Jaek.UI.Actions (
  menuActions
 ,defActions
 ,newAction
 ,openAction
 ,saveAction
 ,importAction
 ,propsAction
 ,quitAction
 ,renderAction
 ,deleteAction
 ,muteAction
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

saveAction :: IO Action
saveAction = actionNew "SaveAction" "Save" Nothing Nothing

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

-- -------------------------------------------
-- editing actions

deleteAction :: IO Action
deleteAction = actionNew "DeleteAction" "Delete" Nothing Nothing

muteAction :: IO Action
muteAction = actionNew "MuteAction" "Mute" Nothing Nothing
