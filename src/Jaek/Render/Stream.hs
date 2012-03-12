{-# LANGUAGE FlexibleContexts #-}

module Jaek.Render.Stream (
  renderPeaks
)

where

import           Prelude as P

import           Jaek.Base
import           Jaek.Peaks

import           Diagrams.Prelude hiding (First)

import qualified Data.Vector.Unboxed as U
import           Data.Int
import           Data.Monoid

renderPeaks ::
  (Renderable (Path R2) b)
  => Int                       -- ^ width
  -> U.Vector Peak             -- ^ peaks
  -> QDiagram b R2 (First TreePath)
renderPeaks _w pk =
  let (l,h) = U.unzip $ U.map (\(Pk b t) -> (pk2Double b,pk2Double t)) pk
      toDgr = fmap (const (First Nothing))
              . stroke
              . fromVertices
              . P.zipWith (curry p2) [0,1..]
              . U.toList
  in  toDgr l `atop` toDgr h

pk2Double :: Int16 -> Double
pk2Double i = fI i / fI (maxBound :: Int16)
