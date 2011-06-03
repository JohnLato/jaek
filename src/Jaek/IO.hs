module Jaek.IO (
  newFileExpr
)

where

import Jaek.Base
import Jaek.StreamExpr

import Sound.Iteratee

import Control.Monad.Error

-- | generate initial StreamExpr from an audio file
newFileExpr :: String -> IO (Either String [StreamExpr])
newFileExpr fp = runErrorT $ do
  (af, scount) <- annMaybe ("Couldn't read file: " ++ fp) $ getAudioInfo fp
  let nc     = numberOfChannels af
      frames = scount `div` nc
  return $ map (\i -> FileSource fp af (fI i) 0 (fI frames)) [0..nc-1]
