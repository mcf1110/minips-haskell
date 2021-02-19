module Lib (showFile, readFromFile) where

import qualified Data.ByteString as B
import qualified Data.List.Split as S
import qualified Data.Word as W
import Numeric (showHex)

toWord32 :: [W.Word8] -> W.Word32
toWord32 ws = toEnum $ sum $ zipWith (*) is [2^(x*8) | x <- [3,2..0]]
  where is = reverse $ fromEnum <$> ws

readFromFile :: FilePath -> IO [W.Word32]
readFromFile path = do
  contents <- B.readFile path
  return $ map toWord32 $ S.chunksOf 4 $ B.unpack contents

showFile :: FilePath -> IO ()
showFile path = do
  ws <- readFromFile path
  mapM_ (putStrLn . (flip showHex "")) ws