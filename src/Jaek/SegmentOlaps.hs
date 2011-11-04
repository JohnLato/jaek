module Jaek.SegmentOlaps (
  removeOlaps
)

where

import Jaek.Base
import qualified Data.Map as M

-- | given a list of segments @(start, duration)@, return a list of
-- non-overlapping segments, also @(start, duration)@.
removeOlaps :: [(SampleCount,Duration)] -> [(SampleCount,Duration)]
removeOlaps =
  (fmap . fmap) fI
  . fromMap
  . foldl insertSegment M.empty
  . (fmap . fmap) fI

insertSegment :: (Num a, Ord a) => M.Map a Bool -> (a,a) -> M.Map a Bool
insertSegment m (off,dur) = firstF . lastF $ M.union pre post
 where
  (pre,post') = M.split off m
  (_bk,post)  = M.split (off+dur) post'
  lastPrior   = fmap fst $ M.maxViewWithKey pre
  firstNext   = fmap fst $ M.minViewWithKey post
  -- If lastPrior is True, already in a block, so don't need to set a new flag
  -- if lastPrior is False or doesn't exist, set a new flag at current offset
  -- if firstNext is True or doesn't exist, set a new flag (off+dur, False)
  -- if firstNext is False, don't need to do anything
  lastF = case lastPrior of
    Just (_,True) -> id
    _             -> M.insert off True
  firstF = case firstNext of
    Just (_,False) -> id
    _              -> M.insert (off+dur) False

fromMap :: (Num a, Ord a) => M.Map a Bool -> [(a,a)]
fromMap m = go (M.toList m)
 where
  go ((off,True):(end,False):xs) = (off,end-off):go xs
  go [] = []
  go _  = error "internal error in SegmentOlap.fromMap"
