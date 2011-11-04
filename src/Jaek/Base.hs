{-# LANGUAGE TypeFamilies
            ,DeriveDataTypeable
            ,GeneralizedNewtypeDeriving #-}

module Jaek.Base (
  SampleCount (..)
 ,Duration (..)
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

import Data.AdditiveGroup as X
import Data.AffineSpace   as X
import Data.Data
import qualified Data.Hashable as H
import qualified Data.Digest.Murmur as MH

import Control.Applicative as X
import Control.Monad       as X
import Control.Monad.Error
import Control.Monad.Trans.Class as X

import Debug.Trace
import Text.Printf

newtype SampleCount = SC Int
 deriving (Eq, Show, Ord, Num, Integral, Enum, Real, Data, Typeable
          ,H.Hashable, MH.Hashable)

newtype Duration = D Int
 deriving (Eq, Show, Ord, Num, Integral, Enum, Real, Data, Typeable
          ,H.Hashable, MH.Hashable)

instance AdditiveGroup SampleCount where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance AdditiveGroup Duration where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance AffineSpace SampleCount where
  type Diff SampleCount = Duration
  (SC end) .-. (SC start) = D (end-start)
  (SC off) .+^ (D dur)    = SC (off+dur)

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
