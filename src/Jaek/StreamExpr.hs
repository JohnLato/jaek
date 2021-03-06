{-# LANGUAGE DeriveDataTypeable
            ,DeriveGeneric
            ,FlexibleContexts
            ,NoMonomorphismRestriction
            ,RankNTypes
            ,MultiParamTypeClasses
            ,FlexibleInstances #-}

module Jaek.StreamExpr (
  -- * Types
  StreamExpr (..)
  -- * Functions
  -- ** StreamExpr manipulations
 ,cut
 ,trim
 ,insert
 ,insertSilence
 ,insertRegion
 ,mix
  -- ** consuming StreamExprs
 ,compile
 -- ** Utility functions
 ,getDur
)

where

import           Jaek.Base
import           Jaek.Gen

import           Sound.Iteratee
import           Control.Monad.Trans.Control
import           Control.Monad.IO.Class

import           Data.Iteratee.Iteratee
import qualified Data.Iteratee.ListLike as L
import qualified Data.Vector.Storable as V
import           Data.Generics.Uniplate.Direct

import           Data.Data
import           Data.Digest.Murmur
import qualified Data.Hashable as H

import           Data.List (mapAccumL)
import           GHC.Generics
import           Text.Printf

data StreamExpr =
   FileSource FilePath AudioFormat ChanNum SampleCount Duration
 | GenSource  GenFunc Duration
 | Region StreamExpr SampleCount Duration
 | StreamSeq [StreamExpr]
 | Mix StreamExpr StreamExpr
 deriving (Eq, Show, Data, Typeable, Generic)

instance Uniplate StreamExpr where
  uniplate (FileSource fp af cn off dur) = plate (FileSource fp af cn off dur)
  uniplate (GenSource g dur) = plate (GenSource g dur)
  uniplate (Region expr off dur) = plate Region |* expr |- off |- dur
  uniplate (StreamSeq exprs) = plate StreamSeq ||* exprs
  uniplate (Mix s1 s2) = plate Mix |* s1 |* s2

instance Biplate [StreamExpr] StreamExpr where
  biplate [] = plate []
  biplate (x:xs) = plate (:) |* x ||* xs

instance Hashable StreamExpr where
  hashGen (FileSource fp _af cn off dur) =
    salt 1 `combine` hashGen fp `combine` hashGen cn
    `combine` hashGen off `combine` hashGen dur
  hashGen (GenSource g dur) = salt 2 `combine` hashGen g `combine` hashGen dur
  hashGen (Region expr off dur) = salt 3 `combine`
    hashGen expr `combine` hashGen off `combine` hashGen dur
  hashGen (StreamSeq exprs) = salt 4 `combine` hashGen exprs
  hashGen (Mix s1 s2)       = salt 5 `combine` hashGen s1 `combine` hashGen s2

instance H.Hashable StreamExpr
instance H.Hashable AudioFormat

getDur :: StreamExpr -> Duration
getDur (FileSource _ _ _ _ dur) = dur
getDur (GenSource _ dur)        = dur
getDur (Region _ _  dur)        = dur
getDur (StreamSeq exprs)        = Prelude.sum $ map getDur exprs
getDur (Mix s1 s2)              = min (getDur s1) (getDur s2)

compile ::
  (MonadIO m, MonadBaseControl IO m, Functor m)
  => StreamExpr
  -> Enumerator Vec m a
compile (FileSource fp af chan off dur) i =
  let numChans = fromIntegral $ numberOfChannels af
  in myRun =<< enumAudioIteratee fp (do
       L.drop (fI off*numChans)
       (getChannel numChans chan ><> L.takeUpTo (fI dur)) i )
compile (GenSource gfunc dur) i =
  enumGen gfunc (L.takeUpTo (fI dur) i) >>= myRun
compile (Region expr off dur) i =
  compile expr (L.drop (fI off) >> L.takeUpTo (fI dur) i) >>= myRun
compile (StreamSeq exprs) i = foldr ((>=>) . compile) enumEof exprs i
compile (Mix s1 s2) i       = mergeEnums (compile s1) (compile s2) mixEtee i

myRun
  :: (Functor f, Monad f, Monad m)
  => Iteratee s1 f (Iteratee s m a)
  -> f (Iteratee s m a)
myRun i = either (error "jaek: TODO: implement myRun properly") id <$> tryRun i

mixEtee :: (Functor m, Monad m) => Enumeratee Vec Vec (Iteratee Vec m) a
mixEtee = L.mergeByChunks (V.zipWith (+)) id id

-- ---------------------------
-- stream op functions

-- | cut a section from a StreamExpr
cut :: SampleCount -> Duration -> StreamExpr -> StreamExpr
cut off dur expr =
  let r1 = Region expr 0 (off .-. 0)
      r2 = Region expr (off .+^ dur) (getDur expr - ((off .-. 0) + dur))
  in  traceD (printf "cut (off dur): %d %d" off dur)
             $ cutCleanup $ StreamSeq [r1, r2]

-- | trim the start/end of a StreamExpr, only exposing @dur@ samples from
-- @offset@.  Dual to 'cut'
trim :: SampleCount -> Duration -> StreamExpr -> StreamExpr
trim off dur expr = removeNegDurs . pushdownRegions $ Region expr off dur

-- | insert @StreamExpr a@ into @StreamExpr b@ at point n.
insert :: SampleCount -> StreamExpr -> StreamExpr -> StreamExpr
insert n src dst =
  let r1 = Region dst 0 (n .-. 0)
      r2 = Region dst n (getDur dst - (n .-. 0))
  in  cutCleanup $ StreamSeq [r1, src, r2]

-- | insert a silent region into @StreamExpr@
insertSilence :: SampleCount -> Duration -> StreamExpr -> StreamExpr
insertSilence off dur expr =
  let r1 = Region expr 0 offDur
      sl = GenSource Null dur
      r2 = Region expr off (getDur expr - offDur)
      offDur = off .-. 0
  in  traceD (printf "insertSilence (off dur): %d %d" off dur)
             $ cutCleanup $ StreamSeq [r1, sl, r2]

-- | insert @StreamExpr b@ into @StreamExpr a@ at @dstOff@.
-- 
-- Similar to @insert . trim@, but more efficient.
insertRegion
  :: SampleCount  -- ^ source offset
  -> Duration     -- ^ source duration
  -> SampleCount  -- ^ destination offset
  -> StreamExpr   -- ^ source expression
  -> StreamExpr   -- ^ destination expression
  -> StreamExpr
insertRegion srcOff srcDur dstOff src = insert dstOff (Region src srcOff srcDur)

-- | Mix together two sources for the specified duration.
-- 
-- The destination @StreamExpr@ (last argument) is unaltered before
-- and after the mixed region.
mix
  :: SampleCount  -- ^ source offset
  -> Duration     -- ^ duration
  -> SampleCount  -- ^ destination offset
  -> StreamExpr   -- ^ source expression
  -> StreamExpr   -- ^ destination expression
  -> StreamExpr
mix srcOff dur dstOff src dst =
  let pre  = Region dst 0 dstOffDiff
      post = Region dst (dstOff .+^ dur) (getDur dst - (dstOffDiff+dur))
      dstm = Region dst dstOff dur
      srcm = Region src srcOff dur
      dstOffDiff = dstOff .-. 0
  in cutCleanup $ StreamSeq [pre, Mix srcm dstm,post]

-- ----------------------------------------------
-- some cleanup rules

-- cleanup sequence for cutting
cutCleanup :: StreamExpr -> StreamExpr
cutCleanup = simplifySeqs . removeNegDurs . commonSeqs . pushdownRegions

removeNegDurs :: StreamExpr -> StreamExpr
removeNegDurs = transform f
 where
  f (GenSource g dur) | dur <= 0 = GenSource g 0
  f x | getDur x <= 0 = GenSource Null 0
  f x = x

-- | remove null streams from sequences, and simplify sequences with 1 source
simplifySeqs :: StreamExpr -> StreamExpr
simplifySeqs = transform f
 where
  f (StreamSeq [s1])  = s1
  f (StreamSeq exprs) = case filter ((> 0) . getDur) exprs of
    []   -> GenSource Null 0
    [s1] -> s1
    ss   -> StreamSeq ss
  f x = x

-- | common-up nested seqs
-- this will flatten all nested seqs, of any depth, into a single seq.
-- it works on multiple depths because transform does a bottom-up rewrite,
-- so at any point the depth is no more than 2
commonSeqs :: StreamExpr -> StreamExpr
commonSeqs = transform f
 where
  f (StreamSeq exprs) = StreamSeq $ childrenBi exprs >>= \f' ->
    case f' of
      StreamSeq exprs' -> exprs'
      x                -> [x]
  f x = x

-- | combine and remove regions when possible
-- remove sources where region offset > source duration
-- THIS IS ONLY WELL-TESTED WITH GENSOURCE AND FILESOURCE; REGION MAY FAIL
pushdownRegions :: StreamExpr -> StreamExpr
pushdownRegions = transform f
 where
  f (Region x off dur)
      | (off .+^ dur) <= 0 = GenSource Null 0
      | fI off > getDur x  = GenSource Null 0
      | otherwise       = case x of
         FileSource fp af c fOff fDur
           | off <= 0  -> FileSource fp af c fOff (min fDur (fI off + dur))
           | otherwise -> FileSource fp af c (fOff+off) (min dur (fDur-fI off))
         gs@(GenSource g gDur)
           | off <= 0  -> GenSource g (min gDur (dur + fI off))
           | otherwise -> Region gs off (min dur (gDur - fI off))
         Region s iOff iDur
           | off < fI iDur -> transform f $
                Region s (iOff+off) (min dur (iDur-fI off))
           | otherwise  -> GenSource Null 0
         Mix s1 s2 -> Mix
             (pushdownRegions (Region s1 off dur))
             (pushdownRegions (Region s2 off dur))
         StreamSeq exprs -> StreamSeq . snd $ mapAccumL mf 0 exprs
          where
            mf pos x' = (pos .+^ getDur x', transform f $
                           Region x' (off-pos) dur)
         _ -> Region x off (min dur (getDur x - fI off))
  f x = x
    -- Offsets may temporarily be set to <0 within this function,
    -- but they should all be >=0 by the point of return.
