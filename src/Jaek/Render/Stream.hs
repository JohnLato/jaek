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
  => SampleCount               -- ^ offset
  -> SampleCount               -- ^ dur
  -> Int                       -- ^ width
  -> U.Vector Peak             -- ^ peaks
  -> AnnDiagram b R2 (First TreePath)
renderPeaks off dur w pk =
  let (l,h) = U.unzip $ U.map (\(Pk b t) -> (b,t)) pk
      getIx i = ((fI i * fI dur `div` w) + fI off) `div` pksz
      -- draw only even pixels.  Get a more clear image this way.
      -- I'll give it a try at any rate...
      rvec vec = U.generate (w `div` 2) (\i ->
                   let ix = getIx (2*i)
                   in  if ix < 0 || ix >= U.length vec
                         then 0
                         else pk2Double $ vec U.! ix)
      toDgr = fmap (const (First Nothing))
              . stroke
              . fromVertices
              . P.zipWith (curry P) [0,2..]
              . U.toList
              . rvec
  in  toDgr l `atop` toDgr h

pk2Double :: Int16 -> Double
pk2Double i = fI i / fI (maxBound :: Int16)
