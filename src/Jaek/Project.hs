-- | data structure, serialization, and constants pertaining to projects
module Jaek.Project (
  peakDir
 ,readProject
 ,writeProject
)

where

import           Jaek.Base
import           Jaek.Project.Parse
import           Jaek.Project.Serialize
import           Jaek.Tree

import           Data.Iteratee
import           Data.Attoparsec.Iteratee
import qualified Data.ByteString.Lazy as L
import           Blaze.ByteString.Builder (toLazyByteString)

peakDir :: FilePath
peakDir = "Images"

-- | only very basic support at the moment, but there really isn't any
-- project data to serialize yet except the tree.
writeProject :: FilePath -> TreeZip -> IO ()
writeProject fp tz = L.writeFile fp . toLazyByteString . build $ fromZipper tz

readProject :: FilePath -> IO TreeZip
readProject fp = zipper <$> fileDriver (parserToIteratee jparse) fp
