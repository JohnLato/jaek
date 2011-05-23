{-# LANGUAGE DeriveDataTypeable #-}

module Jaek.Tree (
  TreePath
 ,NodeRef (..)
 ,HTree
 ,TreeZip
 ,StreamT (..)
 ,initialZipper
 ,goToRef
 -- ** user functions
 ,mkCut
)

where

import           Jaek.Gen
import           Jaek.StreamExpr

import           Data.Generics.Uniplate.Direct
import           Data.Generics.Uniplate.Zipper

import           Data.Data
import           Data.Maybe
import           Data.Tree

type TreePath = [Int]

data NodeRef =
   AbsPath String TreePath
 | RelPath Int TreePath
 deriving (Eq, Show, Data, Typeable)

-- | stream transformers.  Could also have just a single function
-- with type StreamExpr -> StreamExpr, but that would be hard to
-- serialize.
-- 
-- these really should go in their own module...
data StreamT =
   Cut    ChanNum         SampleCount SampleCount
 | Insert ChanNum NodeRef SampleCount SampleCount SampleCount
 | Mix    ChanNum NodeRef SampleCount SampleCount SampleCount
 deriving (Eq, Show, Data, Typeable)

-- | Apply a stream transform to a list of StreamExprs, matching channels
applyTransform :: [StreamExpr] -> StreamT -> [StreamExpr]
applyTransform expr (Cut chn off dur) = modifyListAt chn (cut off dur) expr
applyTransform _ _ = error "applyTransform not fully implemented"

modifyListAt :: Int -> (a -> a) -> [a] -> [a]
modifyListAt n f xs = let (h,t) = splitAt n xs in h ++ [f (head t)] ++ tail t 

-- This is not the same as the Data.Tree.Node constructor.  Instead, it's the
-- label which is used inside a Tree (the payload at the Node).
data Node =
   Init String [StreamExpr]
 | Mod TreePath [StreamT] [StreamExpr]
 deriving (Eq, Show, Data, Typeable)

-- | the number of channels in a node
numChans :: Node -> Int
numChans (Init _ chns)  = length chns
numChans (Mod _ _ chns) = length chns

getExprs :: Node -> [StreamExpr]
getExprs (Init _ chns)  = chns
getExprs (Mod _ _ chns) = chns

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

-- | Construct the initial zipper from a set of StreamExprs (one per channel)
-- and a label
initialZipper :: String -> [StreamExpr] -> TreeZip
initialZipper lbl chans = zipper $ Node (Init lbl chans) []

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
newChildPath (Node (Init _ _) children)     = [length children]
newChildPath (Node (Mod path _ _) children) = path ++ [length children]

-- | Go to a reference within the current zipper
-- for an absolute path, checks that the label matches
goToRef :: NodeRef -> TreeZip -> Maybe TreeZip
goToRef (AbsPath lb pth) zp = case fromZipper zp of
  t@(Node (Init lb' cs) _) | lb == lb' -> followPath pth $ zipper t
  _                                    -> Nothing
goToRef (RelPath pp pth) zp =
  (foldr (>=>) return (replicate pp up) >=> followPath pth) zp

-- ------------------------
-- primary user functions

-- | Perform a cut in the current node in the specified channels.
-- The new child is in focus after this operation.
-- If no valid channels are specified, the zipper is unchanged.
mkCut :: [Int] -> SampleCount -> SampleCount -> TreeZip -> TreeZip
mkCut chns off dur zp =
  let cur@(Node nd children) = hole zp
      streamTs = map (\cn -> Cut cn off dur) $ validateChans nd chns
      strExpr' = Mod (newChildPath cur)
                     streamTs
                     (foldl applyTransform (getExprs nd) streamTs)
      node'    = Node nd (children ++ [Node strExpr' [] ])
  in  case streamTs of
        [] -> zp
        _  -> fromMaybe (error "internal error in mkCut") $
                followPath [last $ newChildPath cur] (replaceHole node' zp)
