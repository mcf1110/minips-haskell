module Lib.Print where

import           Lib.Decode
import           Lib.Memory
import           Lib.Registers
import           Lib.Segment
import           Lib.State

import qualified Data.BitVector   as BV
import qualified Data.IntMap.Lazy as IM
import qualified Data.Vector      as V

import           Data.Char        (toLower)
import           Data.List        (intercalate)
import           Data.Maybe       (fromMaybe)
import           Text.Printf      (printf)

-- MEMORY
showMemory :: Memory -> [String]
showMemory m =
  [ "\t┌─────────────────────────┐"
  , "\t│          Memory         │"
  , "\t├────────────┬────────────┤"
  , "\t│    Addr    │    Code    │"
  , "\t╞════════════╪════════════╡"
  ] <>
  (map line $ IM.assocs m) <> ["\t└────────────┴────────────┘"]
  where
    line (addr, val) = printf "\t│ 0x%08x │ 0x%08x │" addr val

printMemory :: Memory -> IO ()
printMemory = (mapM_ putStrLn) . showMemory

-- REGISTERS
regNumberToNameMap :: IM.IntMap String
regNumberToNameMap =
  IM.fromAscList $
  zip [0 ..] $
  [ "$zero"
  , "$at"
  , "$v0"
  , "$v1"
  , "$a0"
  , "$a1"
  , "$a2"
  , "$a3"
  , "$t0"
  , "$t1"
  , "$t2"
  , "$t3"
  , "$t4"
  , "$t5"
  , "$t6"
  , "$t7"
  , "$s0"
  , "$s1"
  , "$s2"
  , "$s3"
  , "$s4"
  , "$s5"
  , "$s6"
  , "$s7"
  , "$t8"
  , "$t9"
  , "$k0"
  , "$k1"
  , "$gp"
  , "$sp"
  , "$fp"
  , "$ra"
  , "pc"
  , "hi"
  , "lo"
  ]

rName :: Enum a => a -> String
rName = (IM.!) regNumberToNameMap . fromEnum

showRegisters :: Registers -> [String]
showRegisters r =
  [ "\t┌──────────────────────────┐"
  , "\t│         Registers        │"
  , "\t├────────┬────┬────────────┤"
  , "\t│  Name  │ No │    Val     │"
  , "\t╞════════╪════╪════════════╡"
  ] <>
  (map line $ V.toList $ V.indexed r) <> ["\t└────────┴────┴────────────┘"]
  where
    line (num, val) = printf "\t│ %6s │ %02d │ 0x%08x │" (rName num) num val

printRegisters :: Registers -> IO ()
printRegisters = (mapM_ putStrLn) . showRegisters

-- STATE
printState :: State -> IO ()
printState (r, m) = do
  mapM_ putStrLn $ zipWithDefault (showMemory m) (showRegisters r)
  -- based on https://stackoverflow.com/a/21350444
  where
    memSpacing = '\t' : (replicate 23 ' ') <> "\t"
    infPad ls = (map Just ls) <> (repeat Nothing)
    zipWithDefault xs ys =
      map (\(x, y) -> (fromMaybe memSpacing x <> fromMaybe "" y)) $
      takeWhile (/= (Nothing, Nothing)) $ zip (infPad xs) (infPad ys)

-- ASSEMBLY
printProgram :: Program -> IO ()
printProgram = mapM_ (putStrLn . showInstruction)

showInstruction :: Instr -> String
showInstruction ins@(RInstr funct rs rt rd shamt)
  | funct `elem` [Jr] = mkIns [rName rs]
  | funct `elem` [Srl, Sll] = mkIns [rName rd, rName rt, show $ BV.nat shamt]
  | otherwise = mkIns [rName rd, rName rs, rName rt]
  where
    mkIns ls = (toLower <$> show funct) <> " " <> intercalate ", " ls
showInstruction ins@(IInstr op rs rt rd)
  | op `elem` [Lui] = mkIns [rName rt, hx rd]
  | op `elem` [Beq, Bne] = mkIns [rName rs, rName rt, show $ BV.int rd]
  | op `elem` [Lw] =
    mkIns [rName rt, (show $ BV.int rd) <> "(" <> rName rs <> ")"]
  | otherwise = mkIns [rName rt, rName rs, show $ BV.int rd]
  where
    mkIns ls = (toLower <$> show op) <> " " <> intercalate ", " ls
    hx = printf "0x%08x" . BV.int
showInstruction ins@(JInstr op tgt) =
  (toLower <$> show op) <> " " <> (printf "0x%08x" $ BV.int tgt)
showInstruction Syscall = "syscall"
