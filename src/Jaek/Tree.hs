{-# LANGUAGE DeriveDataTypeable #-}

module Jaek.Tree (
  TreePath
 ,NodeRef (..)
 ,HTree
 ,TreeZip
 ,initialZipper
 ,newSource
 ,goToRef
 -- ** user functions
 ,mkCut
)

where

import           Jaek.Gen
import           Jaek.StreamExpr
import           Jaek.StreamT

import           Data.Generics.Uniplate.Direct
import           Data.Generics.Uniplate.Zipper

import           Data.Data
import           Data.Maybe
import           Data.Tree

-- This is not the same as the Data.Tree.Node constructor.  Instead, it's the
-- label which is used inside a Tree (the payload at the Node).
data Node =
   Root
 | Init String TreePath [StreamExpr]
 | Mod TreePath [StreamT] [StreamExpr]
 deriving (Eq, Show, Data, Typeable)

-- | the number of channels in a node
numChans :: Node -> Int
numChans Root            = 0
numChans (Init _ _ chns) = length chns
numChans (Mod _ _ chns)  = length chns

getExprs :: Node -> [StreamExpr]
getExprs Root            = []
getExprs (Init _ _ chns) = chns
getExprs (Mod _ _ chns)  = chns

-- | filter a list so only valid channels are included
validateChans :: Node -> [Int] -> [Int]
validateChans nd = filter (\i -> i >= 0 && i < numChans nd)

instance Uniplate Node where
  uniplate nd = plate nd

-- | The main history tree type
type HTree = Tree Node

-- need the uniplate instance for zipper ops
instance Uniplate (Tree a) where
  uniplate (Node lbl sf) = plate Node |- lbl ||* sf

type TreeZip = Zipper HTree HTree

-- |An initial zipper.  Contains only a Root node.
initialZipper = zipper $ Node Root []

-- | Add a new source (top-level node) from a list of
-- StreamExprs (one per channel) and a label
newSource :: String -> [StreamExpr] -> TreeZip -> TreeZip
newSource lbl chans zp =
  let child pth = Init lbl pth chans
      (tree,newpos) = addChild child $ fromZipper zp
  in fromMaybe zp . followPath [newpos] $ zipper tree

-- | generate a path to the currently-focused node
getPath :: TreeZip -> TreePath
getPath zp = case hole zp of
  (Node (Mod path _ _) _) -> path
  _                       -> []    -- at head of zipper

followPath :: TreePath -> TreeZip -> Maybe TreeZip
followPath []     = Just
followPath (x:xs) =
  (down >=> foldr (>=>) return (replicate x right)) >=> followPath xs

-- | given a tree, generate a path to a new child to the right of the current
-- children.
newChildPath :: HTree -> TreePath
newChildPath (Node Root children)            = [length children]
newChildPath (Node (Init _ path _) children) = path ++ [length children]
newChildPath (Node (Mod path _ _) children)  = path ++ [length children]

-- Given an HTree, add a child with the correct TreePath.
-- returns the position of the new child.
addChild :: (TreePath -> Node) -> HTree -> (HTree, Int)
addChild gen t@(Node nd children) =
  let path = newChildPath t
  in (Node nd (children ++ [Node (gen path) []]), last path)

-- | Go to a reference within the current zipper
goToRef :: NodeRef -> TreeZip -> Maybe TreeZip
goToRef (AbsPath pth)    = followPath pth . zipper . fromZipper
goToRef (RelPath pp pth) =
  (foldr (>=>) return (replicate pp up) >=> followPath pth)

-- | Apply a stream transform to a list of StreamExprs, matching channels
applyTransform :: [StreamExpr] -> StreamT -> [StreamExpr]
applyTransform expr (Cut chn off dur) = modifyListAt chn (cut off dur) expr
applyTransform _ _ = error "applyTransform not fully implemented"

modifyListAt :: Int -> (a -> a) -> [a] -> [a]
modifyListAt n f xs = let (h,t) = splitAt n xs in h ++ [f (head t)] ++ tail t

-- ------------------------
-- primary user functions

-- | Perform a cut in the current node in the specified channels.
-- The new child is in focus after this operation.
-- If no valid channels are specified, the zipper is unchanged.
mkCut :: [Int] -> SampleCount -> SampleCount -> TreeZip -> TreeZip
mkCut chns off dur zp =
  let cur@(Node nd children) = hole zp
      streamTs = map (\cn -> Cut cn off dur) $ validateChans nd chns
      strExpr' pth = Mod pth streamTs
                     (foldl applyTransform (getExprs nd) streamTs)
      (node', pos) = addChild strExpr' cur
  in  case streamTs of
        [] -> zp
        _  -> fromMaybe (error "internal error in mkCut") $
                followPath [pos] (replaceHole node' zp)
