{-# LANGUAGE MultiParamTypeClasses
            ,TypeFamilies
            ,FlexibleContexts #-}

module Jaek.Peaks (
  Peak (..)
 ,pksz
 ,genPeakFile
 ,readPeakFileNonBlocking
)

where

import           Prelude as P

import           Jaek.Base
import           Jaek.Project
import           Jaek.StreamExpr

import           Data.Iteratee as I
import           Blaze.ByteString.Builder

import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as U

import qualified Data.ByteString as BS
import           Data.Monoid
import           Data.Int
import           Data.Bits

import           Control.Exception
import           GHC.Float (double2Int)
import           System.Directory
import           System.FilePath
import           System.IO

-- | A peak chunk, Pk low high
data Peak = Pk {-# UNPACK #-} !Int16 {-# UNPACK #-} !Int16

pksz :: Int
pksz = 4096

updatePeak :: Peak -> Double -> Peak
updatePeak (Pk l h) x = Pk (min l x') (max h x')
 where
  x' = fI . double2Int $ x * fI (maxBound :: Int16)

-- | instead of using a lockfile, I could also explicitly pass locks around.
-- We'll see how it goes, but I'll probably stick with this for now.
genPeakFile :: FilePath -> StreamExpr -> IO ()
genPeakFile fp expr =
  bracket opener
          closer
          $ \h -> run =<< (compile expr $ joinI $
            I.group pksz ><> I.mapStream (V.foldl' updatePeak mempty)
            $ writePkStream h)
 where
  lockfile = peakDir </> fp <.> "lck"
  opener = do
    h <- openBinaryFile (peakDir </> fp) WriteMode
    writeFile lockfile ""
    return h
  closer h = do
    hClose h
    removeFile lockfile

readPeakFileNonBlocking :: FilePath -> IO (Maybe (U.Vector Peak))
readPeakFileNonBlocking fp = do
  locked <- doesFileExist lockfile
  if locked then return Nothing else do
    bs <- BS.readFile fp
    let l = BS.length bs  -- length in bytes, each Peak is 2 Int16s = 4 bytes
    return . Just $ U.generate (l `div` 4) (\i ->
      let ix = 4*i
      in Pk (toInt16 (BS.index bs ix)     (BS.index bs (ix+1)))
            (toInt16 (BS.index bs (ix+2)) (BS.index bs (ix+3)))
      )
 where
  lockfile = peakDir </> fp <.> "lck"
  toInt16 l h = fI $ (fI l :: Int) + shiftL (fI h) 8

writePkStream :: Handle -> Iteratee [Peak] IO ()
writePkStream h = do
  cs <- joinI $ I.take 2048 stream2list
  let bld = fromWriteList
              (\(Pk l h) -> writeInt16le l `mappend` writeInt16le h)
              cs
  liftIO $ toByteStringIO (BS.hPut h) bld
  
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
