module Lib.Registers where
import Text.Printf (printf)
import qualified Data.IntMap.Lazy as IM

import qualified Data.Word as W
import qualified Data.Vector as V


type Registers = V.Vector W.Word32

startingRegisters :: Registers
startingRegisters = V.replicate 35 0 V.// 
                    [(29, 0x7fffeffc), (28, 0x10008000), (32, 0x00400000)]

numberToName :: IM.IntMap String
numberToName = IM.fromAscList $ zip [0..] $ 
                ["$zero", "$at", "$v0", "$v1"
                ,"$a0" ,"$a1" ,"$a2" ,"$a3"
                ,"$t0" ,"$t1" ,"$t2" ,"$t3"
                ,"$t4" ,"$t5" ,"$t6" ,"$t7"
                ,"$s0" ,"$s1" ,"$s2" ,"$s3"
                ,"$s4" ,"$s5" ,"$s6" ,"$s7"
                ,"$t8" ,"$t9" ,"$k0" ,"$k1"
                ,"$gp" ,"$sp" ,"$fp" ,"$ra"
                ,"pc" ,"hi" ,"lo"
                ]


showRegisters :: Registers -> [String]
showRegisters r = 
    ["\t┌──────────────────────────┐"
    ,"\t│         Registers        │"
    ,"\t├────────┬────┬────────────┤"
    ,"\t│  Name  │ No │    Val     │"
    ,"\t╞════════╪════╪════════════╡"
    ] <> (map line $ V.toList $ V.indexed r) <> 
    ["\t└────────┴────┴────────────┘"]
    where line (num, val) = printf "\t│ %6s │ %02d │ 0x%08x │" (numberToName IM.! num) num val

printRegisters :: Registers -> IO ()
printRegisters = (mapM_ putStrLn) . showRegisters