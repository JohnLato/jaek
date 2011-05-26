{-# LANGUAGE DeriveDataTypeable #-}

-- | This module defines the HTree (HistoryTree) type and associated zipper and
-- operations.  An HTree will always have the form
-- 
-- >                                    Root
-- >  (Init "file1" [0] (FileSource ...)) (Init "file2" [1] (FileSource ...))
-- >  (Mod ...)  (Mod ...)                (Mod ...)
-- 
-- i.e. A root node at the top, with 0 or more @Init@ childs.  Every other
-- node in the tree is a @Mod@ below one of the Init's.
-- 
module Jaek.Tree (
  TreePath
 ,NodeRef (..)
 ,HTree
 ,TreeZip
 ,initialZipper
 ,newSource
 ,goToRef
 ,getPath
 -- ** user functions
 ,mkCut
 ,mkInsert
 ,mkMix
)

where

import           Jaek.Gen
import           Jaek.StreamExpr as E
import           Jaek.StreamT as T

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

-- | Filter a list of (srcChan, dstChan) so only valid 'dstChan'
-- channels are included
validateChanP :: Node -> [(Int,Int)] -> [(Int,Int)]
validateChanP nd = filter (\(_,i) -> i >= 0 && i < numChans nd)

instance Uniplate Node where
  uniplate = plate

-- | The main history tree type
type HTree = Tree Node

-- need the uniplate instance for zipper ops
instance Uniplate (Tree a) where
  uniplate (Node lbl sf) = plate Node |- lbl ||* sf

type TreeZip = Zipper HTree HTree

-- |An initial zipper.  Contains only a Root node.
initialZipper :: TreeZip
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
followPath = foldr (\x ->
   (>=>) (down >=> foldr (>=>) return (replicate x right)))
  Just

-- | given a tree, generate a path to a new child to the right of the current
-- childs.
newChildPath :: HTree -> TreePath
newChildPath (Node Root childs)            = [length childs]
newChildPath (Node (Init _ path _) childs) = path ++ [length childs]
newChildPath (Node (Mod path _ _) childs)  = path ++ [length childs]

-- Given an HTree, add a child with the correct TreePath.
-- returns the position of the new child.
addChild :: (TreePath -> Node) -> HTree -> (HTree, Int)
addChild gen t@(Node nd childs) =
  let path = newChildPath t
  in (Node nd (childs ++ [Node (gen path) []]), last path)

-- | Go to a reference within the current zipper
goToRef :: NodeRef -> TreeZip -> Maybe TreeZip
goToRef (AbsPath pth)    = followPath pth . zipper . fromZipper
goToRef (RelPath pp pth) =
  foldr (>=>) return (replicate pp up) >=> followPath pth

-- | Get the StreamExpr from the tree at the specified path and channel
getExpr :: NodeRef -> ChanNum -> TreeZip -> Maybe StreamExpr
getExpr ref chn zp = do
  z' <- goToRef ref zp
  let exprs = getExprs $ rootLabel $ hole z'
  if chn >= 0 && chn < length exprs
    then Just $ exprs !! chn
    else Nothing

-- | Apply a stream transform to a list of StreamExprs, matching channels
applyTransform :: TreeZip -> [StreamExpr] -> StreamT -> [StreamExpr]
applyTransform _ expr (Cut chn off dur) = modifyListAt chn (cut off dur) expr
applyTransform z expr (Insert dstChn ref srcChn dstOff srcOff dur) =
  case getExpr ref srcChn z of
    Nothing  -> []
    Just src -> modifyListAt dstChn (insertRegion srcOff dur dstOff src) expr
applyTransform z expr (T.Mix dstChn ref srcChn dstOff srcOff dur) =
  case getExpr ref srcChn z of
    Nothing  -> []
    Just src -> modifyListAt dstChn (mix srcOff dur dstOff src) expr
applyTransform _ _ _ = error "applyTransform not fully implemented"

modifyListAt :: Int -> (a -> a) -> [a] -> [a]
modifyListAt n f xs = let (h,t) = splitAt n xs in h ++ [f (head t)] ++ tail t


-- ------------------------
-- ------------------------
-- helpers to construct user functions

mod1 :: String -> [Int] -> (ChanNum -> StreamT) -> TreeZip -> TreeZip
mod1 nm chns gen zp =
  let cur@(Node nd _childs) = hole zp
      streamTs = map gen $ validateChans nd chns
      strExpr' pth = Mod pth streamTs
                     (foldl (applyTransform zp) (getExprs nd) streamTs)
      (node', pos) = addChild strExpr' cur
  in  case streamTs of
        [] -> zp
        _  -> fromMaybe (error $ "internal error in " ++ nm) $
                followPath [pos] (replaceHole node' zp)

mod2
  :: String                          -- ^ Name of function
  -> [(Int,Int)]                     -- ^ (srcChn, dstChn)
  -> (ChanNum -> ChanNum -> StreamT) -- ^ srcChan -> dstChan -> StreamT
  -> TreeZip
  -> TreeZip
mod2 nm chns gen zp =
  let cur@(Node nd _childs) = hole zp
      streamTs = map (uncurry gen) $ validateChanP nd chns
      strExpr' pth = Mod pth streamTs
                     (foldl (applyTransform zp) (getExprs nd) streamTs)
      (node', pos) = addChild strExpr' cur
  in  case streamTs of
        [] -> zp
        _  -> fromMaybe (error $ "internal error in " ++ nm) $
                followPath [pos] (replaceHole node' zp)

-- ------------------------
-- primary user functions

-- | Perform a cut in the current node in the specified channels.
-- The new child is in focus after this operation.
-- If no valid channels are specified, the zipper is unchanged.
mkCut :: [Int] -> SampleCount -> SampleCount -> TreeZip -> TreeZip
mkCut chns off dur = mod1 "mkCut" chns (\cn -> Cut cn off dur)

-- | Perform an insert at the current node.
mkInsert
  :: [(Int,Int)]   -- ^ (srcChn, dstChn)
  -> SampleCount   -- ^ source Offset
  -> SampleCount   -- ^ source duration
  -> SampleCount   -- ^ destination offset
  -> NodeRef       -- ^ reference to source expression
  -> TreeZip
  -> TreeZip
mkInsert chns srcOff dur dstOff srcref =
  mod2 "mkInsert" chns (\srcChn dstChn ->
    Insert dstChn srcref srcChn dstOff srcOff dur)

-- | Perform a mix at the current node.
mkMix
  :: [(Int,Int)]   -- ^ (srcChn, dstChn)
  -> SampleCount   -- ^ source Offset
  -> SampleCount   -- ^ source duration
  -> SampleCount   -- ^ destination offset
  -> NodeRef       -- ^ reference to source expression
  -> TreeZip
  -> TreeZip
mkMix chns srcOff dur dstOff srcref =
  mod2 "mkMix" chns (\srcChn dstChn ->
    T.Mix dstChn srcref srcChn dstOff srcOff dur)
