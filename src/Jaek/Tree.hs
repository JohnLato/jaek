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
 ,Node (..)
 ,NodeRef (..)
 ,HTree
 ,TreeZip
 ,ClipRef
 ,iZip
 ,newSource
 ,goToHead
 ,goToRef
 ,nodePath
 ,getPath
 -- ** user node functions
 ,numChans
 ,getExprs
 ,getExprs'
 ,getTransforms
 ,getTransforms'
 ,liftT
 ,liftZ
 -- ** user tree manipulation functions
 ,mkCut
 ,mkMute
 ,mkInsert
 ,mkMix
 ,module Data.Generics.Uniplate.Zipper
)

where

import           Jaek.Gen
import           Jaek.StreamExpr as E
import           Jaek.StreamT as T

import           Data.Generics.Uniplate.Direct
import           Data.Generics.Uniplate.Zipper
import           Data.Tuple.Select

import           Data.Data
import           Data.Maybe
import           Data.Tree

-- | Reference to a section of audio
type ClipRef = (Int, SampleCount, Duration)

-- This is not the same as the Data.Tree.Node constructor.  Instead, it's the
-- label which is used inside a Tree (the payload at the Node).
data Node =
   Root
 | Init String TreePath [StreamExpr]
 | Mod TreePath [StreamT] [StreamExpr]
 deriving (Eq, Show, Data, Typeable)

-- | lift a "function on a node" to a "fuction on a tree".
liftT :: (Node -> a) -> HTree -> a
liftT f (Node dt _) = f dt

-- | lift a "function on a tree" to a
--  "function on current position in the zipper"
liftZ :: (HTree -> a) -> TreeZip -> a
liftZ f = f . hole

-- | the number of channels in a node
numChans :: Node -> Int
numChans Root            = 0
numChans (Init _ _ chns) = length chns
numChans (Mod _ _ chns)  = length chns

getExprs  :: HTree -> [StreamExpr]
getExprs = liftT getExprs'

getExprs' :: Node -> [StreamExpr]
getExprs' Root            = []
getExprs' (Init _ _ chns) = chns
getExprs' (Mod _ _ chns)  = chns

getTransforms :: HTree -> [StreamT]
getTransforms = liftT getTransforms'

getTransforms' :: Node -> [StreamT]
getTransforms' (Mod _ cs _) = cs
getTransforms' _ = []

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
iZip :: TreeZip
iZip = zipper $ Node Root []

-- | Add a new source (top-level node) from a list of
-- StreamExprs (one per channel) and a label
newSource :: String -> [StreamExpr] -> TreeZip -> TreeZip
newSource lbl chans zp =
  let child pth = Init lbl pth chans
      (tree,newpos) = addChild child $ fromZipper zp
  in fromMaybe zp . followPath [newpos] $ zipper tree

nodePath :: Node -> TreePath
nodePath (Mod path _ _)  = path
nodePath (Init _ path _) = path
nodePath _               = []

-- | generate a path to the currently-focused node
getPath :: TreeZip -> TreePath
getPath zp = let (Node nd _) = hole zp in nodePath nd

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

-- | Go to the head reference of the zipper
goToHead :: TreeZip -> TreeZip
goToHead = zipper . fromZipper

-- | Go to a reference within the current zipper
goToRef :: NodeRef -> TreeZip -> Maybe TreeZip
goToRef (AbsPath pth)    = followPath pth . zipper . fromZipper
goToRef (RelPath pp pth) =
  foldr (>=>) return (replicate pp up) >=> followPath pth

-- | Get the StreamExpr from the tree at the specified path and channel
getExpr :: NodeRef -> ChanNum -> TreeZip -> Maybe StreamExpr
getExpr ref chn zp = do
  z' <- goToRef ref zp
  let exprs = getExprs' $ rootLabel $ hole z'
  if chn >= 0 && chn < length exprs
    then Just $ exprs !! chn
    else Nothing

-- | Apply a stream transform to a list of StreamExprs, matching channels
applyTransform :: TreeZip -> [StreamExpr] -> StreamT -> [StreamExpr]
applyTransform _ expr (Cut chn off dur) = modifyListAt chn (cut off dur) expr
applyTransform _ expr (Mute chn off dur) =
  modifyListAt chn (insertSilence off dur . cut off dur) expr
applyTransform z expr (Insert dstChn ref srcChn dstOff srcOff dur) =
  case getExpr ref srcChn z of
    Nothing  -> []
    Just src -> modifyListAt dstChn (insertRegion srcOff dur dstOff src) expr
applyTransform z expr (T.Mix dstChn ref srcChn dstOff srcOff dur) =
  case getExpr ref srcChn z of
    Nothing  -> []
    Just src -> modifyListAt dstChn (mix srcOff dur dstOff src) expr

modifyListAt :: Int -> (a -> a) -> [a] -> [a]
modifyListAt n f xs = let (h,t) = splitAt n xs in h ++ [f (head t)] ++ tail t


-- ------------------------
-- ------------------------
-- helpers to construct user functions

mod1 :: String -> [Int] -> (ChanNum -> [StreamT]) -> TreeZip -> TreeZip
mod1 nm chns gen zp =
  let cur = hole zp
      streamTs = concatMap gen $ liftT validateChans cur chns
      strExpr' pth = Mod pth streamTs
                     (foldl (applyTransform zp) (getExprs cur) streamTs)
      (node', pos) = addChild strExpr' cur
  in  case streamTs of
        [] -> zp
        _  -> fromMaybe (error $ "internal error in " ++ nm) $
                followPath [pos] (replaceHole node' zp)

mod2
  :: String                            -- ^ Name of function
  -> [(Int,Int)]                       -- ^ (srcChn, dstChn)
  -> (ChanNum -> ChanNum -> [StreamT]) -- ^ srcChan -> dstChan -> [StreamT]
  -> TreeZip
  -> TreeZip
mod2 nm chns gen zp =
  let cur = hole zp
      streamTs = concatMap (uncurry gen) $ liftT validateChanP cur chns
      strExpr' pth = Mod pth streamTs
                     (foldl (applyTransform zp) (getExprs cur) streamTs)
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
mkCut :: [ClipRef] -> TreeZip -> TreeZip
mkCut sels = mod1 "mkCut" (map sel1 sels) $ \cn ->
  map (\(_,off,dur) -> Cut cn off dur) $ filter (\i -> sel1 i == cn) sels

-- | Mute the current node in the specific locations
mkMute :: [ClipRef] -> TreeZip -> TreeZip
mkMute sels = mod1 "mkMute" (map sel1 sels) $ \cn ->
  map (\(_,off,dur) -> Mute cn off dur) $ filter (\i -> sel1 i == cn) sels

-- | Perform an insert at the current node.
mkInsert
  :: [(Int,Int)]   -- ^ (srcChn, dstChn)
  -> SampleCount   -- ^ source Offset
  -> Duration      -- ^ source duration
  -> SampleCount   -- ^ destination offset
  -> NodeRef       -- ^ reference to source expression
  -> TreeZip
  -> TreeZip
mkInsert chns srcOff dur dstOff srcref =
  mod2 "mkInsert" chns (\srcChn dstChn ->
    [Insert dstChn srcref srcChn dstOff srcOff dur])

-- | Perform a mix at the current node.
mkMix
  :: [(Int,Int)]   -- ^ (srcChn, dstChn)
  -> SampleCount   -- ^ source Offset
  -> Duration      -- ^ source duration
  -> SampleCount   -- ^ destination offset
  -> NodeRef       -- ^ reference to source expression
  -> TreeZip
  -> TreeZip
mkMix chns srcOff dur dstOff srcref =
  mod2 "mkMix" chns (\srcChn dstChn ->
    [T.Mix dstChn srcref srcChn dstOff srcOff dur])
