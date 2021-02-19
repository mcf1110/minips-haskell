module Lib.Memory where

import qualified Data.IntMap.Lazy as IM
import qualified Data.Word as W
import Text.Printf (printf)

type Memory = IM.IntMap W.Word32

showMemory :: Memory -> [String]
showMemory m =
    ["\t┌─────────────────────────┐"
    ,"\t│          Memory         │"
    ,"\t├────────────┬────────────┤"
    ,"\t│    Addr    │    Code    │"
    ,"\t╞════════════╪════════════╡"
    ] <> (map line $ IM.assocs m) <>
    ["\t└────────────┴────────────┘"]
    where line (addr, val) =  printf "\t│ 0x%08x │ 0x%08x │" addr val

printMemory :: Memory -> IO ()
printMemory = (mapM_ putStrLn) . showMemory