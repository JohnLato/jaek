module Jaek.UI.Input.Action (

)

where

import Graphics.UI.Gtk
import Jaek.Tree
import Jaek.UI.Input.Drags

import Reactive.Banana

-- keyactOnSelect
keyactOnSelect tz sz vm sels key
  | keyToChar key == Just 'm' = Just $ mkMute regions tz
  | otherwise                 = Nothing
  
 where
  regions = concatMap (dragToRegions sz tz vm) sels
