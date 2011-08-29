module Jaek.UI.Controllers.EditModes (
  EditMode (..)
)

where

import Jaek.UI.Controllers.Base

data EditMode =
    None
  | Pencil
  deriving (Eq, Show, Ord, Enum)


