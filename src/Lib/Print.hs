module Lib.Print where

import Lib.Memory
import Lib.Registers
import Lib.Segment
import Lib.State
import Lib.Decode

import qualified Data.IntMap.Lazy as IM
import qualified Data.Vector as V
import qualified Data.BitVector as BV

import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.List (intercalate)

-- MEMORY

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

-- REGISTERS

regNumberToNameMap :: IM.IntMap String
regNumberToNameMap = IM.fromAscList $ zip [0..] $ 
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
regNumberToName :: Enum a => a -> String
regNumberToName = (IM.!) regNumberToNameMap . fromEnum

showRegisters :: Registers -> [String]
showRegisters r = 
    ["\t┌──────────────────────────┐"
    ,"\t│         Registers        │"
    ,"\t├────────┬────┬────────────┤"
    ,"\t│  Name  │ No │    Val     │"
    ,"\t╞════════╪════╪════════════╡"
    ] <> (map line $ V.toList $ V.indexed r) <> 
    ["\t└────────┴────┴────────────┘"]
    where line (num, val) = printf "\t│ %6s │ %02d │ 0x%08x │" (regNumberToName num) num val

printRegisters :: Registers -> IO ()
printRegisters = (mapM_ putStrLn) . showRegisters

-- STATE

printState :: State -> IO ()
printState (r,m) = do
  mapM_ putStrLn $ zipWithDefault (showMemory m) (showRegisters r)
  where
  -- based on https://stackoverflow.com/a/21350444
  memSpacing = '\t':(replicate 23 ' ') <> "\t"
  infPad ls = (map Just ls) <> (repeat Nothing)
  zipWithDefault xs ys = 
    map (\(x,y) -> (fromMaybe memSpacing x <> fromMaybe "" y)) $ 
      takeWhile (/= (Nothing, Nothing)) $ 
      zip (infPad xs) (infPad ys)


-- ASSEMBLY
printProgram :: Program -> IO ()
printProgram = mapM_ (putStrLn . showInstruction)

showInstruction :: Instr -> String
showInstruction ins@(RInstr funct rs rt rd _) = 
    (toLower <$> show funct) <> " " <> intercalate ", " [regNumberToName rd, regNumberToName rs, regNumberToName rt]
showInstruction ins@(IInstr op rs rt rd) 
    | op `elem` [Lui] = mkIns [regNumberToName rt, hx rd]
    | op `elem` [Ori]  = mkIns [regNumberToName rt, regNumberToName rs, hx rd]
    | otherwise = mkIns [regNumberToName rt, regNumberToName rs, show $ BV.int rd]
    where 
        mkIns ls = (toLower <$> show op) <> " " <> intercalate ", " ls
        hx = printf "0x%08x" . BV.int
showInstruction ins@(JInstr _ _ ) = "J"
showInstruction Syscall = "syscall"