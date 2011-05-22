{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction
            ,RankNTypes
            ,MultiParamTypeClasses
            ,FlexibleInstances #-}

module Jaek.StreamExpr (
  -- * Types
  StreamExpr (..)
  -- ** re-exported types
 ,GenFunc
 -- * Functions
 -- ** StreamExpr manipulations
 ,cut
 ,insert
 -- ** consuming StreamExprs
 ,compile
)

where

import           Jaek.Base

import           Sound.Iteratee
import           Data.Iteratee.Iteratee
import qualified Data.Iteratee.ListLike as L
import qualified Data.Vector.Storable as V
import           Data.Generics.Uniplate.Direct

import           Data.Data
import           Data.List (mapAccumL)
import           Control.Monad.CatchIO

type Vec = V.Vector Double

-- | Temporary, this should go in a separate module too
data GenFunc = Null deriving (Eq, Show, Data,Typeable)

enumGen :: Monad m => GenFunc -> Enumerator Vec m a
enumGen Null =
  let nullChunk = V.replicate defaultBufSize 0
      nullList  = nullChunk : nullList
  in  enumList nullList 

data StreamExpr =
   FileSource FilePath AudioFormat ChanNum SampleCount SampleCount
                                                       -- ^ frame offset, dur
 | GenSource  GenFunc SampleCount                      -- ^ dur
 | Region StreamExpr SampleCount SampleCount           -- ^ offset, dur
 | StreamSeq [StreamExpr]
 | Mix StreamExpr StreamExpr
 deriving (Eq, Show, Data, Typeable)

instance Uniplate StreamExpr where
  uniplate (FileSource fp af cn off dur) = plate (FileSource fp af cn off dur)
  uniplate (GenSource g dur) = plate (GenSource g dur)
  uniplate (Region expr off dur) = plate Region |* expr |- off |- dur
  uniplate (StreamSeq exprs) = plate StreamSeq ||* exprs
  uniplate (Mix s1 s2) = plate Mix |* s1 |* s2

instance Biplate [StreamExpr] StreamExpr where
  biplate [] = plate []
  biplate (x:xs) = plate (:) |* x ||* xs

getDur :: StreamExpr -> SampleCount
getDur (FileSource _ _ _ _ dur) = dur
getDur (GenSource _ dur)        = dur
getDur (Region _ _  dur)        = dur
getDur (StreamSeq exprs)        = Prelude.sum $ map getDur exprs
getDur (Mix s1 s2)              = min (getDur s1) (getDur s2)

compile ::
  (MonadCatchIO m, Functor m)
  => StreamExpr
  -> Enumerator Vec m a
compile (FileSource fp af chan off dur) i =
  let numChans = fromIntegral $ numberOfChannels af
  in enumAudioIteratee fp $ do
       L.drop (off*numChans)
       joinI $ (getChannel numChans chan <>< L.takeUpTo dur) i
compile (GenSource gfunc dur) i = enumGen gfunc $ joinI (L.takeUpTo dur i)
compile (Region expr off dur) i = compile expr
                                    (L.drop off >> joinI (L.takeUpTo dur i))
compile (StreamSeq exprs) i = foldr (>=>) enumEof (map compile exprs) i
compile (Mix s1 s2) i =
  let (e1, e2) = (compile s1, compile s2)
  in mergeEnums e1 e2 mixEtee i

mixEtee :: (Functor m, Monad m) => Enumeratee Vec Vec (Iteratee Vec m) a
mixEtee = L.mergeByChunks (V.zipWith (+)) id id

-- ---------------------------
-- stream op functions

-- | cut a section from a StreamExpr
cut :: SampleCount -> SampleCount -> StreamExpr -> StreamExpr
cut off dur expr =
  let r1 = Region expr 0 off
      r2 = Region expr (off+dur) (getDur expr - (off+dur))
  in  cutCleanup $ StreamSeq [r1, r2]

-- | insert @StreamExpr b@ into @StreamExpr a@ at point n.
insert :: SampleCount -> StreamExpr -> StreamExpr -> StreamExpr
insert n expr insertand =
  let r1 = Region expr 0 n
      r2 = Region expr n (getDur expr - n)
  in  cutCleanup $ StreamSeq [r1, insertand, r2]

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
pushdownRegions = transform f
 where
  f (Region x off dur)
      | off + dur <= 0  = GenSource Null 0
      | off > getDur x  = GenSource Null 0
      | otherwise       = case x of
         FileSource fp af c fOff fDur
           | off <= 0  -> FileSource fp af c fOff (min fDur (dur + off))
           | otherwise -> FileSource fp af c (fOff+off) (min dur (fDur-off))
         gs@(GenSource g gDur)
           | off <= 0  -> GenSource g (min gDur (dur + off))
           | otherwise -> Region gs off (min dur (gDur - off))
         Region s iOff iDur
           | off < iDur -> transform f $
                Region s (iOff+off) (min dur (iDur-off))
           | otherwise  -> GenSource Null 0
         Mix s1 s2 -> Mix
             (pushdownRegions (Region s1 off dur))
             (pushdownRegions (Region s2 off dur))
         StreamSeq exprs -> StreamSeq . snd $ mapAccumL mf 0 exprs
          where
            mf spos x = (getDur x + spos, transform f $ Region x (off-spos) dur)
         _ -> Region x off (min dur (getDur x - off))
  f x = x
    -- Offsets may temporarily be set to <0 within this function,
    -- but they should all be >=0 by the point of return.
