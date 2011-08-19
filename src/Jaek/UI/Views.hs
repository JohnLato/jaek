-- | define a map from Node -> View, to keep track of what's in view.
--   not much in here, but I expect it'll expand in the future.
module Jaek.UI.Views (
   ViewMap
  ,View (..)
  ,ViewChange (..)
  ,isWaveView
  ,iMap
  ,mapFromTree
  ,getView
  ,updateMap
  ,slideX
  ,slideY
)

where

import           Jaek.Base
import           Jaek.StreamExpr (getDur)
import           Jaek.Tree

import qualified Data.HashMap.Strict as M

import           Data.List (foldl')
import           Data.Maybe
import           Data.Tree

type ViewMap = M.HashMap TreePath View

data ViewChange =
    NewDoc
  | AddSrc
  | MdNode
  | ModView View
  deriving (Eq, Show)

-- | Information about what's currently in view...
data View =
    FullView !Double !Double !Double !Double   -- ^ xScale, yScale, xOff, yOff
  | WaveView !SampleCount !SampleCount -- ^ streamOff, streamDur
  deriving (Eq, Show)

isWaveView :: View -> Bool
isWaveView (WaveView _ _) = True
isWaveView _              = False

updateMap :: TreeZip -> ViewChange -> ViewMap -> ViewMap
updateMap _z NewDoc _m = iMap
updateMap zp AddSrc mp = addWaveNode zp mp
updateMap zp MdNode mp = addWaveNode zp mp
updateMap zp (ModView view) mp = changeWaveNode zp view mp

iMap :: ViewMap
iMap = mapFromTree $ fromZipper iZip

-- | Return either the view for a node, or the default view
getView :: ViewMap -> HTree -> View
getView m (Node node _) = fromMaybe def $ M.lookup (nodePath node) m
 where
  def
   | node == Root = FullView 1 1 0 0
   | otherwise    = WaveView 0 $ foldl' max 0 . map getDur $ getExprs' node

-- | Create a new map with a default view for everything in the tree.
mapFromTree :: HTree -> ViewMap
mapFromTree = M.fromList . map uf . flatten
 where
  uf Root = ([], FullView 1 1 0 0)
  uf node = (nodePath node,
             WaveView 0 $ foldl' max 0 . map getDur $ getExprs' node)

addWaveNode :: TreeZip -> ViewMap -> ViewMap
addWaveNode tz mp = M.insert (liftT nodePath cur) (WaveView 0 srcdur) mp
 where
  cur    = hole tz
  srcdur = foldl' max 0 . map getDur $ getExprs cur

changeWaveNode :: TreeZip -> View -> ViewMap -> ViewMap
changeWaveNode tz v mp = M.adjust (const v) (liftT nodePath $ hole tz) mp

slideX :: Double -> View -> View
slideX dist (FullView xs ys xOff yOff) = FullView xs ys (xOff+dist) yOff
slideX dist (WaveView off dur) = WaveView nOff dur
 where
   nOff = off + (round $ dist * fI dur)

slideY :: Double -> View -> View
slideY dist (FullView xs ys xOff yOff) = FullView xs ys xOff (yOff+dist)
slideY _    waveView                   = waveView
