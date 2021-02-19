module Lib.Memory where

import qualified Data.IntMap.Lazy as IM
import qualified Data.Word as W
import Text.Printf (printf)

type Memory = IM.IntMap W.Word32

showMemory :: Memory -> IO ()
showMemory m = do
    putStrLn "\t┌────────────┬────────────┐"
    putStrLn "\t│    Addr    │    Code    │"
    putStrLn "\t╞════════════╪════════════╡"
    mapM_ (\(addr, val) -> putStrLn $ printf "\t│ 0x%08x │ 0x%08x │" addr val) $ IM.assocs m
    putStrLn "\t└────────────┴────────────┘"