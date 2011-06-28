-- | define a map from Node -> View, to keep track of what's in view.
--   not much in here, but I expect it'll expand in the future.
module Jaek.UI.Views (
   ViewMap
  ,View (..)
  ,ViewChange (..)
  ,isWaveView
  ,iMap
  ,mapFromTree
  ,updateMap
)

where

import           Jaek.Base
import           Jaek.StreamExpr (getDur)
import           Jaek.Tree

import qualified Data.HashMap.Strict as M

import           Data.List (foldl')
import           Data.Tree (flatten)

type ViewMap = M.HashMap TreePath View

data ViewChange =
    NewDoc
  | AddSrc
  deriving (Eq, Show)

-- | Information about what's currently in view...
data View =
    FullView !Double !Double           -- ^ xScale, yScale
  | WaveView !SampleCount !SampleCount -- ^ streamOff, streamDur
  deriving (Eq, Show)

isWaveView :: View -> Bool
isWaveView (WaveView _ _) = True
isWaveView _              = False

updateMap :: TreeZip -> ViewChange -> ViewMap -> ViewMap
updateMap _z NewDoc _m = iMap
updateMap zp AddSrc mp = addSource zp mp

iMap :: ViewMap
iMap = mapFromTree $ fromZipper iZip

-- | Create a new map with a default view for everything in the tree.
mapFromTree :: HTree -> ViewMap
mapFromTree = M.fromList . map uf . flatten
 where
  uf Root = ([], FullView 1 1)
  uf node = (nodePath node,
             WaveView 0 $ foldl' max 0 . map getDur $ getExprs' node)

addSource :: TreeZip -> ViewMap -> ViewMap
addSource tz mp = M.insert (liftT nodePath cur) (WaveView 0 srcdur) mp
 where
  cur    = hole tz
  srcdur = foldl' max 0 . map getDur $ getExprs cur
