{-# LANGUAGE MultiParamTypeClasses
            ,TypeFamilies
            ,FlexibleContexts #-}

module Jaek.Render.Stream (
  exprPeaks
)

where

import           Prelude as P

import           Jaek.Base
import           Jaek.StreamExpr

import           GHC.Float (double2Int)

import           Diagrams.Prelude
import           Data.Iteratee as I

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as U

import           Data.Monoid
import           Data.Int

data Peak = Pk {-# UNPACK #-} !Int16 {-# UNPACK #-} !Int16

pksz :: Int
pksz = 4096

-- | create a Peak diagram from a StreamExpr
exprPeaks ::
 (Renderable (Path R2) b)
  => SampleCount
  -> SampleCount
  -> Int
  -> StreamExpr
  -> IO (AnnDiagram b R2 (First TreePath))
exprPeaks off dur w expr = renderPeaks off dur w <$> genPeaks expr

updatePeak :: Peak -> Double -> Peak
updatePeak (Pk l h) x = Pk (min l x') (max h x')
 where
  x' = fI . double2Int $ x * fI (maxBound :: Int16)

genPeaks :: StreamExpr -> IO (U.Vector Peak)
genPeaks expr = do
  i <- compile expr $ joinI $
        I.group pksz ><> I.mapStream (V.foldl' updatePeak mempty) $ stream2list
  U.fromList <$> run i

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

-- need an Unboxed instance for this one...
instance Monoid Peak where
  mempty = Pk 0 0
  mappend (Pk l1 h1) (Pk l2 h2) = Pk (min l1 l2) (max h1 h2)

newtype instance U.MVector s Peak = MV_Peak (U.MVector s (Int16, Int16))
newtype instance U.Vector Peak = V_Peak (U.Vector (Int16, Int16))

instance M.MVector U.MVector Peak where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Peak v) = M.basicLength v
  basicUnsafeSlice i n (MV_Peak v) = MV_Peak $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Peak v1) (MV_Peak v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Peak `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Pk x y) = MV_Peak `liftM` M.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Peak v) i = uncurry Pk `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Peak v) i (Pk x y) = M.basicUnsafeWrite v i (x,y)
  basicClear (MV_Peak v) = M.basicClear v
  basicSet (MV_Peak v) (Pk x y) = M.basicSet v (x,y)
  basicUnsafeCopy (MV_Peak v1) (MV_Peak v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Peak v) n = MV_Peak `liftM` M.basicUnsafeGrow v n

instance G.Vector U.Vector Peak where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Peak v) = V_Peak `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Peak v) = MV_Peak `liftM` G.basicUnsafeThaw v
  basicLength (V_Peak v) = G.basicLength v
  basicUnsafeSlice i n (V_Peak v) = V_Peak $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Peak v) i
                = uncurry Pk `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Peak mv) (V_Peak v)
                = G.basicUnsafeCopy mv v
  elemseq _ (Pk x y) z = G.elemseq (undefined :: U.Vector a) x
                       $ G.elemseq (undefined :: U.Vector a) y z

instance U.Unbox Peak
