module Lib.Print where

import           Lib.Computer
import           Lib.Decode
import           Lib.Memory
import           Lib.Registers
import           Lib.Segment

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
  [ "┌─────────────────────────┐"
  , "│          Memory         │"
  , "├────────────┬────────────┤"
  , "│    Addr    │    Code    │"
  , "╞════════════╪════════════╡"
  ] <>
  map line (filter (\(k, _) -> k >= 0x10010000 && k < 0x7fffef1c) (IM.assocs m)) <>
  ["└────────────┴────────────┘"]
  where
    line (addr, val) = printf "│ 0x%08x │ 0x%08x │" addr val

showFullMemory :: Memory -> [String]
showFullMemory m =
  [ "┌─────────────────────────┐"
  , "│          Memory         │"
  , "├────────────┬────────────┤"
  , "│    Addr    │    Code    │"
  , "╞════════════╪════════════╡"
  ] <>
  map line (IM.assocs m) <> ["└────────────┴────────────┘"]
  where
    line (addr, val) = printf "│ 0x%08x │ 0x%08x │" addr val

printMemory :: Memory -> IO ()
printMemory = mapM_ putStrLn . showMemory

-- REGISTERS
regNumberToNameMap :: IM.IntMap String
regNumberToNameMap =
  IM.fromAscList $
  zip
    [0 ..]
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

coProcRegNumberToNameMap :: IM.IntMap String
coProcRegNumberToNameMap =
  IM.fromAscList $ zip [0 ..] (map (("$f" <>) . show) [0 .. 31])

fName :: Enum a => a -> String
fName = (IM.!) coProcRegNumberToNameMap . fromEnum

showRegisters :: Registers -> [String]
showRegisters r =
  [ "\t┌──────────────────────────┐"
  , "\t│         Registers        │"
  , "\t├────────┬────┬────────────┤"
  , "\t│  Name  │ No │    Val     │"
  , "\t╞════════╪════╪════════════╡"
  ] <>
  map line (V.toList $ V.indexed r) <> ["\t└────────┴────┴────────────┘"]
  where
    line (num, val) = printf "\t│ %6s │ %02d │ 0x%08x │" (rName num) num val

printRegisters :: Registers -> IO ()
printRegisters = mapM_ putStrLn . showRegisters

-- STATE
printComputer :: Computer -> IO ()
printComputer (r, m) = do
  mapM_ putStrLn $ zipWithDefault (showMemory m) (showRegisters r)
  -- based on https://stackoverflow.com/a/21350444
  where
    memSpacing = replicate 23 ' ' <> "\t"
    infPad ls = map Just ls <> repeat Nothing
    zipWithDefault xs ys =
      map (\(x, y) -> fromMaybe memSpacing x <> fromMaybe "" y) $
      takeWhile (/= (Nothing, Nothing)) $ zip (infPad xs) (infPad ys)

-- ASSEMBLY
printProgram :: Program -> IO ()
printProgram = mapM_ (putStrLn . showInstruction)

printProgramWithHexes :: Segment -> IO ()
printProgramWithHexes contents = putStrLn $ unlines $ tab addr $tab hex decoded
  where
    decoded = showInstruction <$> decodeProgram contents
    addr = printf "%08x:" <$> [0x400000 + i * 4 | i <- [0 ..] :: [Integer]]
    hex = printf "%08x" <$> contents
    tab = zipWith (\x y -> x <> "\t" <> y)

showInstruction :: Instr -> String
showInstruction ins@(RInstr funct rs rt rd shamt)
  | funct `elem` [Mult] = mkIns [rName rs, rName rt]
  | funct `elem` [Jr] = mkIns [rName rs]
  | funct `elem` [Jalr] = mkIns [rName rd, rName rs]
  | funct `elem` [Srl, Sll] = mkIns [rName rd, rName rt, show $ BV.nat shamt]
  | otherwise = mkIns [rName rd, rName rs, rName rt]
  where
    mkIns ls = (toLower <$> show funct) <> " " <> intercalate ", " ls
showInstruction ins@(IInstr op rs rt rd)
  | op `elem` [Lui] = mkIns [rName rt, hx rd]
  | op `elem` [Beq, Bne] = mkIns [rName rs, rName rt, dec rd]
  | op `elem` [Lw, Sw] = mkIns [rName rt, dec rd <> "(" <> rName rs <> ")"]
  | otherwise = mkIns [rName rt, rName rs, dec rd]
  where
    mkIns ls = (toLower <$> show op) <> " " <> intercalate ", " ls
    hx = printf "0x%08x" . BV.int
    dec = show . BV.int
showInstruction ins@(JInstr op tgt) =
  (toLower <$> show op) <> " " <> printf "0x%08x" (BV.int tgt)
showInstruction ins@(FRInstr funct fmt ft fs fd)
  | funct `elem` [Mfc1, Mtc1] = mkIns [rName ft, fName fs]
  where
    mkIns ls = (toLower <$> show funct) <> " " <> intercalate ", " ls
showInstruction Syscall = "syscall"
showInstruction Nop = "nop"
showInstruction Break = "break"
showInstruction _ = "XXXXXXXXXXXXXXXXXX"
