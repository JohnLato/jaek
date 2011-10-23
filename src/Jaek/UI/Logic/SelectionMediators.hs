-- | Mediate between the user-provided selections and whatever values
-- are needed for specific functions
module Jaek.UI.Logic.SelectionMediators (
  insertSels
)

where

import Jaek.Base
import Jaek.Tree

import Data.Tuple.Select

import Data.List (sortBy)
import Data.Ord

-- | for inserts, only insert into currently selected channels.
-- only paste channels which begin at the same point
insertSels :: [(NodeRef, [ClipRef])] -> [ClipRef] -> TreeZip -> TreeZip
insertSels []          _          = id
insertSels ((_, []):_) _          = id
insertSels _           []         = id
insertSels ((node, srcs):_) dests = mkInsert chnPairs strt dur dstrt node
 where
  (_, strt, dur) = head $ sortBy (comparing sel2) srcs
  dstrt    = minimum $ map sel2 dests
  filtFn s = map sel1 . filter ((== s) . sel2)
  srcChns  = filtFn strt srcs
  dstChns  = filtFn dstrt dests
  chnPairs = zip srcChns dstChns
