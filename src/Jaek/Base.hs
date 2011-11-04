{-# LANGUAGE DeriveDataTypeable #-}

module Jaek.Base (
  SampleCount
 ,ChanNum
 ,TreePath
 ,NodeRef (..)
 ,fI
 ,liftIO
 ,ignore
 ,annMaybe
 ,tpass
 ,module X
)

where

import Data.Data

import Control.Applicative as X
import Control.Monad       as X
import Control.Monad.Error
import Control.Monad.Trans.Class as X

import Debug.Trace
import Text.Printf

type SampleCount = Int
type ChanNum = Int

type TreePath = [Int]

data NodeRef =
   AbsPath TreePath
 | RelPath Int TreePath
 deriving (Eq, Show, Data, Typeable)

fI :: (Num b, Integral a) => a -> b
fI = fromIntegral

ignore :: IO a -> IO ()
ignore m = m >> return ()

annMaybe :: (Monad m, Functor m) => String -> m (Maybe a) -> ErrorT String m a
annMaybe s m = ErrorT $ fmap (maybe (Left s) Right) m

-- | Adds a trace to `a` with the given label
tpass :: Show a => String -> a -> a
tpass lbl a = trace (printf (lbl ++ ": %s\n") (show a)) a
