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

enumGen :: Monad m => GenFunc -> Enumerator (V.Vector Double) m a
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
  -> (Iteratee (V.Vector Double) m a)
  -> (m (Iteratee (V.Vector Double) m a))
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
  in  e1 $ e2 (joinI . mixEtee $ ilift lift i) >>= run

-- these don't go here either, but I'm leaving them for the moment for testing until I figure out the best place to but them...

mixEtee = convStream mixIter

mixIter :: Monad m => Iteratee (V.Vector Double) (Iteratee (V.Vector Double) m) (V.Vector Double)
mixIter = do
  iChunk <- lift getChunk
  oChunk <- getChunk
  return $ V.zipWith (+) iChunk oChunk

