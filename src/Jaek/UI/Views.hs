-- | define a map from Node -> View, to keep track of what's in view.
--   not much in here, but I expect it'll expand in the future.
module Jaek.UI.Views (
   ViewMap
  ,View (..)
  ,ViewChange (..)
  ,iMap
  ,updateMap
)

where

import           Jaek.Base
import           Jaek.StreamExpr (getDur)
import           Jaek.Tree

import qualified Data.HashMap.Strict as M
import           Data.Generics.Uniplate.Zipper

import           Data.List (foldl')

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


updateMap :: TreeZip -> ViewChange -> ViewMap -> ViewMap
updateMap _z NewDoc _m = iMap
updateMap zp AddSrc mp = addSource zp mp

iMap :: ViewMap
iMap = M.fromList [([], FullView 1 1)]

addSource :: TreeZip -> ViewMap -> ViewMap
addSource tz mp = M.insert (liftT nodePath cur) (WaveView 0 srcdur) mp
 where
  cur    = hole tz
  srcdur = foldl' max 0 . map getDur $ getExprs cur
