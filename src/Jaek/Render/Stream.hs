{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Jaek.Render.Stream (
  exprPeaks
)

where

import           Prelude as P

import           Jaek.Base
import           Jaek.StreamExpr

import           Diagrams.Prelude
import           Data.Iteratee as I

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as V

import           Data.Monoid

data Peak = Pk !Double !Double

pksz :: Int
pksz = 4096

-- | create a Peak diagram from a StreamExpr
exprPeaks ::
 (Renderable (Path R2) b) =>
  StreamExpr
  -> IO (AnnDiagram b R2 (First TreePath))
exprPeaks expr = renderPeaks <$> genPeaks expr

-- need an Unboxed instance for this one...
instance Monoid Peak where
  mempty = Pk 0 0
  mappend (Pk l1 h1) (Pk l2 h2) = Pk (min l1 l2) (max h1 h2)

updatePeak :: Peak -> Double -> Peak
updatePeak (Pk l h) x = Pk (min l x) (max h x)

genPeaks :: StreamExpr -> IO (U.Vector (Double, Double))
genPeaks expr = do
  i <- compile expr $ joinI $
        I.group pksz ><> I.mapStream (V.foldl' updatePeak mempty) $ stream2list
  U.fromList . map (\(Pk l h) -> (l,h)) <$> run i

renderPeaks ::
  (Renderable (Path R2) b)
  => U.Vector (Double,Double)
  -> AnnDiagram b R2 (First TreePath)
renderPeaks pk =
  let (l,h) = U.unzip pk
      toDgr = fmap (const (First Nothing))
              . stroke
              . fromVertices
              . map P
              . P.zip [0..]
              . U.toList
  in  toDgr l `atop` toDgr h
