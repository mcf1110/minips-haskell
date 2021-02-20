module Lib.File
  ( printFile
  , readFile
  ) where

import Numeric (showHex)
import Prelude hiding (readFile)

import qualified Data.ByteString as B
import qualified Data.List.Split as S
import qualified Data.Word as W

import Lib.Segment (Segment)

toWord32 :: [W.Word8] -> W.Word32
toWord32 ws = toEnum $ sum $ zipWith (*) is [2 ^ (x * 8) | x <- [3,2 .. 0]]
  where
    is = reverse $ fromEnum <$> ws

readFile :: FilePath -> IO Segment
readFile path = do
  contents <- B.readFile path
  return $ map toWord32 $ S.chunksOf 4 $ B.unpack contents

printFile :: FilePath -> IO ()
printFile path = do
  ws <- readFile path
  mapM_ (putStrLn . (flip showHex "")) ws
