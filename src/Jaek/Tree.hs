{-# LANGUAGE DeriveDataTypeable #-}

module Jaek.Tree (
  TreePath
 ,NodeRef (..)
)

where

import Jaek.Gen
import Jaek.StreamExpr

import Data.Generics.Uniplate.Direct
import Data.Generics.Uniplate.Zipper

import Data.Data
import Data.Tree

type TreePath = [Int]

data NodeRef =
   AbsPath String TreePath
 | RelPath Int TreePath
 deriving (Eq, Show, Data, Typeable)

-- | stream transformers.  Could also have just a single function
-- with type StreamExpr -> StreamExpr, but that would be hard to
-- serialize.
data StreamT =
   Cut    ChanNum         SampleCount SampleCount
 | Insert ChanNum NodeRef SampleCount SampleCount SampleCount
 | Mix    ChanNum NodeRef SampleCount SampleCount SampleCount
 deriving (Eq, Show, Data, Typeable)

-- This is not the same as the Data.Tree.Node constructor!
data Node =
   Init String [StreamExpr]
 | Mod [StreamT] [StreamExpr]
 deriving (Eq, Show, Data, Typeable)

instance Uniplate Node where
  uniplate nd = plate nd

-- | The main history tree type
type HTree = Tree Node

-- need the uniplate instance for zipper ops
instance Uniplate (Tree a) where
  uniplate (Node lbl sf) = plate Node |- lbl ||* sf

type TreeZip = Zipper HTree HTree

-- | Construct the initial zipper from a set of StreamExprs (one per channel)
-- and a label
initialZipper :: String -> [StreamExpr] -> TreeZip
initialZipper lbl chans = zipper $ Node (Init lbl chans) []

followPath :: TreePath -> TreeZip -> Maybe TreeZip
followPath []     = Just
followPath (x:xs) =
  (down >=> foldr (>=>) return (replicate x right)) >=> followPath xs

-- | Go to a reference within the current zipper
-- for an absolute path, checks that the label matches
goToRef :: NodeRef -> TreeZip -> Maybe TreeZip
goToRef (AbsPath lb pth) zp = case fromZipper zp of
  t@(Node (Init lb' cs) _) | lb == lb' -> followPath pth $ zipper t
  _                                    -> Nothing
goToRef (RelPath pp pth) zp =
  (foldr (>=>) return (replicate pp up) >=> followPath pth) zp

