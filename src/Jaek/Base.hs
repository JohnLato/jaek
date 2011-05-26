{-# LANGUAGE DeriveDataTypeable #-}

module Jaek.Base (
  SampleCount
 ,ChanNum
 ,TreePath
 ,NodeRef (..)
 ,module Control.Applicative
 ,module Control.Monad
 ,module Control.Monad.Trans.Class
 ,ignore
)

where

import Data.Data

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

type SampleCount = Int
type ChanNum = Int

type TreePath = [Int]

data NodeRef =
   AbsPath TreePath
 | RelPath Int TreePath
 deriving (Eq, Show, Data, Typeable)

ignore :: IO a -> IO ()
ignore m = m >> return ()
