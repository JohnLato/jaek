{-# LANGUAGE DeriveDataTypeable #-}

module Jaek.StreamT (
  StreamT (..)
)

where

import           Jaek.Gen
import           Jaek.StreamExpr

import           Data.Data
import           Data.Maybe
import           Data.Tree

-- | stream transformers.  Could also have just a single function
-- with type StreamExpr -> StreamExpr, but that would be hard to
-- serialize.
data StreamT =
   Cut    ChanNum         SampleCount SampleCount
 | Insert ChanNum NodeRef ChanNum SampleCount SampleCount SampleCount
 | Mix    ChanNum NodeRef ChanNum SampleCount SampleCount SampleCount
 deriving (Eq, Show, Data, Typeable)
