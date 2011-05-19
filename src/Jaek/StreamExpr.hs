{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, RankNTypes #-}

module Jaek.StreamExpr (
  -- * Types
  StreamExpr (..)
  -- ** re-exported types
 ,SampleCount
 ,ChanNum
 ,GenFunc
)

where

import           Sound.Iteratee
import           Data.Iteratee
import qualified Data.Iteratee.ListLike as L
import qualified Data.Vector.Storable as V

import           Data.Data
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans.Class

-- | temporary, will import this later
type SampleCount = Int
type ChanNum = Int

-- | Temporary, this should go in a separate module too
data GenFunc = Null deriving (Eq, Show, Data,Typeable)

type Vec = V.Vector Double

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

compile ::
  (MonadCatchIO m, Functor m)
  => StreamExpr
  -> (Iteratee Vec m a)
  -> (m (Iteratee Vec m a))
compile (FileSource fp af chan off dur) i =
  enumAudioIteratee fp $ do
    L.drop (off*numChans)
    joinI $ (getChannel numChans chan <>< takeUpTo dur) i
 where
  numChans = fromIntegral $ numberOfChannels af
compile (GenSource gfunc dur) i = enumGen gfunc $ joinI (L.takeUpTo dur i)
compile (Region expr off dur) i = compile expr
                                    (L.drop off >> joinI (L.takeUpTo dur i))
compile (StreamSeq exprs) i = foldr (>=>) enumEof (map compile exprs) i
compile (Mix s1 s2) i =
  let (e1, e2) = (compile s1, compile s2)
  in mergeEnums e1 e2 mixEtee i

-- | mix two streams.  Maybe this should go into sndfile-enumerators?
mixEtee :: (Functor m, Monad m) => Enumeratee Vec Vec (Iteratee Vec m) a
mixEtee = mergeByChunks (V.zipWith (+)) id id

