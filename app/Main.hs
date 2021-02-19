module Main where

import Lib

import qualified Data.IntMap.Lazy as IM
import qualified Data.Word as W
import Text.Printf (printf)

type Memory = IM.IntMap W.Word32
type Segment = [W.Word32]

loadSegment :: Int -> Segment -> Memory
loadSegment starting = IM.fromAscList . zip addr
    where addr = iterate (+4) starting

showMemory :: Memory -> IO ()
showMemory m = do
    putStrLn "\t┌────────────┬────────────┐"
    putStrLn "\t│    Addr    │    Code    │"
    putStrLn "\t╞════════════╪════════════╡"
    mapM_ (\(addr, val) -> putStrLn $ printf "\t│ 0x%08x │ 0x%08x │" addr val) $ IM.assocs m
    putStrLn "\t└────────────┴────────────┘"

main :: IO ()
main = do
    textSegment <- readFromFile "./inputs/03.input.text"
    dataSegment <- readFromFile "./inputs/03.input.data"
    showMemory $ IM.filter (/= 0) $ loadSegment 0x10010000 dataSegment <> loadSegment 0x00400000 textSegment