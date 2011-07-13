{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction ,RankNTypes #-}

module Jaek.Gen (
  Vec
 ,GenFunc (..)
 ,enumGen
 ,module Jaek.Base
)

where

import           Jaek.Base

import           Sound.Iteratee
import           Data.Iteratee.Iteratee
import           Data.Iteratee.ListLike ()
import qualified Data.Vector.Storable as V

import           Data.Data
import           Data.Digest.Murmur
import qualified Data.Hashable as H

data GenFunc =
   Null
 | ConstF Double
 deriving (Eq, Show, Data, Typeable)

instance Hashable GenFunc where
  hashGen Null       = salt 0x0
  hashGen (ConstF x) = salt 0x1 `combine` hashGen x

instance H.Hashable GenFunc where
  hash Null       = 1
  hash (ConstF x) = 2 `H.combine` H.hash x

type Vec = V.Vector Double

enumGen :: Monad m => GenFunc -> Enumerator Vec m a
enumGen Null =
  let nullChunk = V.replicate defaultBufSize 0
      nullList  = nullChunk : nullList
  in  enumList nullList

