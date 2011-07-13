{-# LANGUAGE TypeSynonymInstances #-}

-- | Serialize a Jaek project file and its various component types.
module Jaek.Project.Serialize (
  Build (..)
)

where

import           Jaek.Base
import           Jaek.Gen
import           Jaek.StreamExpr as SE
import           Jaek.StreamT    as ST
import           Jaek.Tree

import           Sound.Iteratee (AudioFormat (..))
import           Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as U

import qualified Data.ByteString as B
import           Data.Monoid
import           Data.Tree

-- | a helper for working with Unicode strings, since we may not know the
-- size immediately and there's no demarcation of the end.
buildStr :: String -> Builder
buildStr str = build (B.length bs) `mappend` fromByteString bs
  where bs =  toByteString $ U.fromString str

class Build a where
  build :: a -> Builder

instance Build a => Build [a] where
  build xs = mconcat (build (length xs) : map build xs)

instance Build Int where
  build = fromInt64le . fI

instance Build Integer where
  build = fromInt64le . fI

instance Build Double where
  build d = U.fromString (show d) `mappend` U.fromString "z"

instance Build AudioFormat where
  build (AudioFormat nc sr bd) =
    fromWrite . mconcat $ map (writeInt64le . fI) [nc,sr,bd]

instance Build GenFunc where
  build Null       = fromWord8 0
  build (ConstF x) = fromWord8 1 `mappend` build x

instance Build StreamExpr where
  build (FileSource fp af cn off dur) =
    mconcat [fromWord8 0, buildStr fp, build af
            ,build cn, build off, build dur]
  build (GenSource gf sc) = mconcat [fromWord8 1, build gf, build sc]
  build (Region se off dur) =
    mconcat [fromWord8 2, build se, build off, build dur]
  build (StreamSeq ses) = fromWord8 3 `mappend` build ses
  build (SE.Mix se1 se2)   = mconcat [fromWord8 4, build se1, build se2]

instance Build NodeRef where
  build (AbsPath tp)   = fromWord8 0 `mappend` build tp
  build (RelPath i tp) = mconcat [fromWord8 1, build i, build tp]

instance Build StreamT where
  build (Cut cn off dur) =
    mconcat [fromWord8 0, build cn, build off, build dur]
  build (Mute cn off dur) =
    mconcat [fromWord8 1, build cn, build off, build dur]
  build (Insert c1 nr c2 off1 off2 dur) = mconcat [fromWord8 2
    , build c1, build nr, build c2, build off1, build off2, build dur]
  build (ST.Mix c1 nr c2 off1 off2 dur) = mconcat [fromWord8 3
    , build c1, build nr, build c2, build off1, build off2, build dur]

instance Build Node where
  build Root = fromWord8 0
  build (Init fp tp ses) =
    mconcat [fromWord8 1, buildStr fp, build tp, build ses]
  build (Mod tp trans sexprs) =
    mconcat [fromWord8 2, build tp, build trans, build sexprs]

instance Build HTree where
  build (Node dt children) = build dt `mappend` build children
