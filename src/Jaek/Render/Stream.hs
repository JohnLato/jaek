{-# LANGUAGE FlexibleContexts #-}

module Jaek.Render.Stream (
  exprPeaks
)

where

import           Prelude as P

import           Jaek.Base
import           Jaek.Peaks

import           Diagrams.Prelude
import           Data.Iteratee as I

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
      getIx i = ((fI i * dur `div` w) + off) `div` pksz
      rvec vec = U.generate w (\i ->
                   let ix = getIx i
                   in if ix >= U.length vec then 0 else pk2Double $ vec U.! ix)
      toDgr = fmap (const (First Nothing))
              . stroke
              . fromVertices
              . P.zipWith (curry P) [0..]
              . U.toList
              . rvec
  in  toDgr l `atop` toDgr h

pk2Double :: Int16 -> Double
pk2Double i = fI i / fI (maxBound :: Int16)
