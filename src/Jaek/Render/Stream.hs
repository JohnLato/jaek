{-# LANGUAGE FlexibleContexts #-}

module Jaek.Render.Stream (
  renderPeaks
)

where

import           Prelude as P

import           Jaek.Base
import           Jaek.Peaks

import           Diagrams.Prelude

import qualified Data.Vector.Unboxed as U
import           Data.Int

renderPeaks ::
  (Renderable (Path R2) b)
  => Int                       -- ^ width
  -> U.Vector Peak             -- ^ peaks
  -> AnnDiagram b R2 (First TreePath)
renderPeaks _w pk =
  let (l,h) = U.unzip $ U.map (\(Pk b t) -> (pk2Double b,pk2Double t)) pk
      toDgr = fmap (const (First Nothing))
              . stroke
              . fromVertices
              . P.zipWith (curry P) [0,1..]
              . U.toList
  in  toDgr l `atop` toDgr h

pk2Double :: Int16 -> Double
pk2Double i = fI i / fI (maxBound :: Int16)
