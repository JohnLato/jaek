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
import qualified Data.Iteratee.ListLike as L
import qualified Data.Vector.Storable as V

import           Data.Data

data GenFunc = Null deriving (Eq, Show, Data, Typeable)

type Vec = V.Vector Double

enumGen :: Monad m => GenFunc -> Enumerator Vec m a
enumGen Null =
  let nullChunk = V.replicate defaultBufSize 0
      nullList  = nullChunk : nullList
  in  enumList nullList

