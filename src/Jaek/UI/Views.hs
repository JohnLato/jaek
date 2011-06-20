-- | define a map from Node -> View, to keep track of what's in view.
--   not much in here, but I expect it'll expand in the future.
module Jaek.UI.Views (
  NodeViewMap
)

where

import           Jaek.Base
import           Jaek.Render (View (..))

import qualified Data.HashMap.Strict as M

type NodeViewMap = M.HashMap TreePath View

