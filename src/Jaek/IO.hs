module Jaek.IO (
  newFileExpr
 ,renderExprs
)

where

import           Jaek.Base
import           Jaek.StreamExpr

import           Sound.Iteratee
import           Data.Iteratee as I

import qualified Data.Vector.Storable as V

import           Data.List (intersperse)
import           Control.Monad.CatchIO
import           Control.Monad.Error

-- | generate initial StreamExpr from an audio file
newFileExpr :: String -> IO (Either String [StreamExpr])
newFileExpr fp = runErrorT $ do
  (af, scount) <- annMaybe ("Couldn't read file: " ++ fp) $ getAudioInfo fp
  let nc     = numberOfChannels af
      frames = scount `div` nc
  return $ map (\i -> FileSource fp af (fI i) 0 (fI frames)) [0..nc-1]

-- | Render a list of StreamExprs as an interleaved file.
-- 
-- The number of channels specified in the provided AudioFormat
-- is ignored.
renderExprs
  :: WriteInfo
  -> [StreamExpr]
  -> IO ()
renderExprs (fp, fformat, af') exprs = do
   let af = af' {numberOfChannels = fI $ Prelude.length exprs}
   runAudioMonad $ interleaver exprs (getWriter fformat fp af) >>= run
   return ()

interleaver :: (Functor m, MonadCatchIO m) => [StreamExpr] -> Enumerator Vec m a
interleaver []   = error "jaek: can't interleave 0 streams"
interleaver [s1] = compile s1
interleaver [s1,s2] = mergeEnums (compile s1) (compile s2)
                        (mergeByChunks interleave2 l2Stereo r2Stereo)
interleaver l    = error $ "jaek: can't yet interleave "
                           ++ show (Prelude.length l) ++ " streams."

interleave2 :: Vec -> Vec -> Vec
interleave2 l r | V.length l == V.length r =
   V.generate (2*V.length l)
              (\ix -> case ix `divMod` 2 of
                        (ix', 0) -> V.unsafeIndex l ix'
                        (ix', 1) -> V.unsafeIndex r ix')
   -- this could possibly be optimized by doing a zip and ptr-casting,
   -- maybe
   -- or could just convert to lists and combine them
interleave2 _ _ = error "jaek: interleave2: mismatched input sizes"

l2Stereo :: Vec -> Vec
l2Stereo v = V.fromListN (2*V.length v) . (++ [0]) . intersperse 0 $ V.toList v

r2Stereo v = V.fromListN (2*V.length v) . (0:) . intersperse 0 $ V.toList v
