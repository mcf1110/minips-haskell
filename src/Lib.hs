module Lib
    ( someFunc
    ) where

import qualified Data.ByteString as B

someFunc :: FilePath -> IO ()
someFunc path = do
  contents <- B.readFile path
  print $ B.unpack contents
