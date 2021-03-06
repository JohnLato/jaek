{-# LANGUAGE MultiParamTypeClasses
            ,TypeFamilies
            ,FlexibleContexts
            ,TupleSections #-}

module Jaek.Peaks (
  Peak (..)
 ,PathMap
 ,defaultPathMap
 ,pksz
 ,genPeaksForNode
 ,createReadPeaksForNode
)

where

import           Prelude as P

import           Jaek.Base
import           Jaek.Project
import           Jaek.StreamExpr
import           Jaek.Tree

import qualified Data.Iteratee as I

import           Data.Digest.Murmur as Hash
import qualified Data.HashMap.Strict as Map
import           Data.Offset
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Unboxed as U
import qualified Data.ZoomCache as Z
import qualified Data.ZoomCache.Numeric as Z

import qualified Data.ByteString as BS
import           Data.Int
import qualified Data.IntMap as IM
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set

import           Control.Arrow (first)
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import           Control.Monad.State
import           System.Directory
import           System.FilePath
import           Text.Printf

-- | A peak chunk, Pk low high
data Peak = Pk {-# UNPACK #-} !Int16 {-# UNPACK #-} !Int16

pksz :: Int
pksz = 1024

-- A map to the hash values which become peak filepaths
type PathMap = Map.HashMap StreamExpr Hash.Hash

defaultPathMap :: IO (TVar PathMap)
defaultPathMap = newTVarIO Map.empty

-- | get the path for the peak file of a StreamExpr.  If it's not in the map
-- already, return a new map with it added.
genExprPath :: FilePath -> PathMap -> StreamExpr -> (FilePath, Maybe PathMap)
genExprPath root mp str = maybe uRes fromMap $ Map.lookup str mp
 where
  genPath slt = slt + hash str
  used = Set.fromList $ Map.elems mp
  unique = head . dropWhile (`Set.member` used) $ map genPath [0..]
  fromMap hsh = (root </> peakDir </> show hsh <.> "jpk", Nothing)
  uRes = (root </> peakDir </> show unique <.> "jpk"
         , Just $ Map.insert str unique mp)

genPeakPath :: FilePath -> PathMap -> HTree -> ([FilePath], Maybe PathMap)
genPeakPath root mp htree = foldr accf ([], Nothing) $ getExprs htree
 where
  accf expr (paths, mMap) =
    first (:paths) $ genExprPath root (fromMaybe mp mMap) expr

-- | create peak files for the currently-selected node
genPeaksForNode :: FilePath -> TVar PathMap -> TreeZip -> IO ()
genPeaksForNode root mpRef tzip =
  let htree = hole tzip
      exprs = getExprs htree
  in do
    paths <- atomically $ do
      mp <- readTVar mpRef
      let (paths, mp') = genPeakPath root mp htree
      maybe (return ()) (writeTVar mpRef) mp'
      return paths
    mapM_ (uncurry genPeakFile) $ zip paths exprs

-- | read peak files for a node.  If the files don't exist, create them
-- first.
createReadPeaksForNode ::
  FilePath
  -> SampleCount
  -> SampleCount
  -> Int
  -> TVar PathMap
  -> TreeZip
  -> IO [U.Vector Peak]
createReadPeaksForNode root start dur pixcount mpRef tzip =
  let htree = hole tzip
      exprs = getExprs htree
      checkAndRegen (path, expr) = do
        when debug $ putStrLn $ "checking for peak: " ++ path
        needsRegen <- not <$> doesFileExist path
        when needsRegen $ do
          when debug $ putStrLn ("needs regen " ++ path)
          genPeakFile path expr
      doStream (path, expr) = fmap snd $ Thread.forkIO $ do
        checkAndRegen (path, expr)
        readPeakFile path start dur pixcount
  in do
    paths <- atomically $ do
      mp <- readTVar mpRef
      let (paths, mp') = genPeakPath root mp htree
      maybe (return ()) (writeTVar mpRef) mp'
      return paths
    results <- mapM doStream $ zip paths exprs
    mapM (Thread.result =<<) results

-- | create a peak file for a stream.  Very rudimentary support at present.
genPeakFile :: FilePath -> StreamExpr -> IO ()
genPeakFile fp expr = do
  zh <- opener
  zh' <- execStateT (compile expr (I.mapM_ (Z.write 1)) >>= I.run) zh
  closer zh'
 where
  tm = IM.singleton 1
         $ Z.setCodec (0 :: Double) $ Z.TrackSpec (Z.Codec (0::Double))
                  False False Z.ConstantSR (fI pksz) BS.empty
  lockfile = fp <.> "lck"
  opener = do
    when debug $ putStrLn $ "peak file name: " ++ fp
    h <- Z.openWrite tm Nothing False fp
    when debug $ putStrLn $ "creating lockfile: " ++ lockfile
    writeFile lockfile ""
    when debug $ putStrLn "lockfile created"
    return h
  closer h = do
    Z.closeWrite h
    removeFile lockfile
    when debug $ putStrLn $ "removing lockfile: " ++ lockfile

readPeakFileNonBlocking ::
  FilePath
  -> SampleCount
  -> SampleCount
  -> Int
  -> IO (Maybe (U.Vector Peak))
readPeakFileNonBlocking fp start' dur pixcount = do
  locked <- doesFileExist lockfile
  when debug $ putStrLn $ printf "start: %d\ndur: %d\npixcount: %d\nzoomsz: %d\nzoomLevel: %d\nsampsPerPixel: %d" (fI start'::Int) (fI dur :: Int) pixcount zoomSz zoomLevel sampsPerPixel
  if locked then return Nothing else Just <$>
    I.fileDriver (I.joinI $ I.mapChunks (Offset 0)
      I.><> Z.enumCacheFile Z.standardIdentifiers
      -- this technically creates a small thunk, but the greater efficiency
      -- in terms of consuming the associated bytestring makes up for it.
      I.><> I.group 64
      I.><> I.mapChunks concat
      I.><> Z.filterTracks [1]
      I.><> Z.enumSummaryLevel zoomLevel
      $ procSummary) fp
 where
  lockfile = fp <.> "lck"
  d2i d = floor $ d * fI (maxBound :: Int16)
  sumToPeak :: Z.ZoomSummary -> Peak
  sumToPeak (Z.ZoomSummary zSum') =
    let Just zSum = Z.toSummaryDouble zSum'
    in  Pk (d2i . Z.numMin $ Z.summaryData zSum) (d2i . Z.numMax $ Z.summaryData zSum)
  -- return @pixcount@ peak chunks from @start@ for @dur@
  -- already measured for rendering
  procSummary = do
    I.drop skipFrames
    buf <- liftIO $ M.unsafeNew pixcount
    liftIO $ M.set buf (Pk 0 0)
    I.takeUpTo numFrames I.=$
      I.foldM (updateFrame buf) (startIx,startCount)
    liftIO $ U.unsafeFreeze buf
  updateFrame buf (ix,count) frame =
    let (bump,count') = (count+zoomSz) `divMod` sampsPerPixel
        newIx         = bump+ix
    in  when (bump == 1) (writeFn buf newIx $! sumToPeak frame)
          >> return (newIx, count')

  writeFn = if debug then M.write else M.unsafeWrite
    
  -- if start is negative, just work from 0
  -- the starting index needs to be updated thought
  start   = if start' < 0 then 0 else start'
  startIx = if start' >= 0
              then -1
              else (fI (negate start') `div` sampsPerPixel) - 1
  startCount = skipFrames*zoomSz `rem` sampsPerPixel
  sampsPerPixel = fI dur `div` pixcount
  -- if the zoomLevel == 0, then have to use the soundfile data
  zoomLevel  = floor $ logBase 2 (fI $ 1 + (sampsPerPixel `div` pksz) :: Double)
  zoomSz     = 2 ^ (max 0 $ zoomLevel-1) * pksz
  skipFrames = fI (start `div` fI zoomSz)
  numFrames  = min (fI dur `div` zoomSz) -- total frames available for duration
                (((sampsPerPixel * (pixcount-(1+startIx))) - startCount) `div` zoomSz)

readPeakFile ::
  FilePath
  -> SampleCount
  -> SampleCount
  -> Int
  -> IO (U.Vector Peak)
readPeakFile fp start dur pixcount = go
 where
  go = do
    res <- readPeakFileNonBlocking fp start dur pixcount
    case res of
      Just v -> return v
      Nothing -> threadDelay 400 >> go

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
