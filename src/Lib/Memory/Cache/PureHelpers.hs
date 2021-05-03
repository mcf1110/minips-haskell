module Lib.Memory.Cache.PureHelpers where

import qualified Data.IntMap.Lazy   as IM
import qualified Data.Vector        as V

import           Data.Maybe         (fromMaybe, isNothing)
import           Optics             ((^.))

import           Lib.Computer.Types

getCacheMapIndex :: Enum i => CacheMap -> i -> Maybe Int
getCacheMapIndex cm ix = V.findIndex (maybe False hasAddress) ways
  where
    nInt = fromEnum ix
    line = lineNumber ix cm
    ways = getWaysAtLine line cm
    withoutOffset = nInt `div` (cm ^. wordsPerLine * 4)
    hasAddress v = (v ^. address) == withoutOffset

lineNumber :: Enum i => i -> CacheMap -> Int
lineNumber ix cm = ifCacheWasInfinite `mod` linesInCache
  where
    addr = fromEnum ix
    linesInCache = cm ^. nLines
    wordsInALine = cm ^. wordsPerLine
    bytesInAWord = 4
    ifCacheWasInfinite = addr `div` (bytesInAWord * wordsInALine)

selectIndexThroughStrategy ::
     CacheStrategy -> V.Vector (Maybe CacheLine) -> [Int] -> (Int, [Int])
selectIndexThroughStrategy Random ways (r:rs) = (abs r `mod` V.length ways, rs)
selectIndexThroughStrategy LRU ways rng =
  (V.maxIndex $ V.mapMaybe (fmap (^. lastUsed)) ways, rng)
    --mapMaybe is equivalent to map, because none of the positions will be empty anyway

getWaysAtLine :: Int -> CacheMap -> V.Vector (Maybe CacheLine)
getWaysAtLine block cm =
  fromMaybe (V.replicate (cm ^. nWays) Nothing) $ (cm ^. addresses) IM.!? block

selectIndex ::
     V.Vector (Maybe CacheLine) -> CacheStrategy -> [Int] -> ([Int], Int)
selectIndex w0 str rng = fromMaybe strategyIndex <$> (rng', firstEmptyIndex)
  where
    firstEmptyIndex = V.findIndex isNothing w0
    (strategyIndex, rng') = selectIndexThroughStrategy str w0 rng
