module Lib.Print where

import           Lib.Decode
import           Lib.Registers
import           Lib.Segment

import qualified Data.BitVector     as BV
import qualified Data.IntMap.Lazy   as IM
import qualified Data.Vector        as V

import           Data.Char          (isUpper, toLower)
import           Data.List          (intercalate)
import           Data.List.Split    (keepDelimsL, split, whenElt)
import           Data.Maybe         (fromMaybe)
import           Data.Time          (UTCTime, diffUTCTime, getCurrentTime)
import           Lib.Computer.Types
import           Optics             (Lens', view, (^.))
import           Text.Printf        (printf)

-- MEMORY
showMemory :: Memory -> [String]
showMemory m =
  [ "┌─────────────────────────┐"
  , "│          Memory         │"
  , "├────────────┬────────────┤"
  , "│    Addr    │    Code    │"
  , "╞════════════╪════════════╡"
  ] <>
  map line (filter (\(k, _) -> k >= 0x00800000 && k < 0x7fffef1c) (IM.assocs m)) <>
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
  map line (V.toList $ V.indexed $ r ^. gpr) <>
  ["\t└────────┴────┴────────────┘"]
  where
    line (num, val) = printf "\t│ %6s │ %02d │ 0x%08x │" (rName num) num val

printRegisters :: Registers -> IO ()
printRegisters = mapM_ putStrLn . showRegisters

-- STATE
printComputer :: Computer -> IO ()
printComputer comp = do
  mapM_ putStrLn $
    zipWithDefault (showMemory $ comp ^. mem) (showRegisters $ comp ^. reg)
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
  | funct `elem` [Mult, Div] = mkIns [rName rs, rName rt]
  | funct `elem` [Mflo, Mfhi] = mkIns [rName rd]
  | funct `elem` [Jr] = mkIns [rName rs]
  | funct `elem` [Jalr] = mkIns [rName rd, rName rs]
  | funct `elem` [Srl, Sra, Sll] =
    mkIns [rName rd, rName rt, show $ BV.nat shamt]
  | otherwise = mkIns [rName rd, rName rs, rName rt]
  where
    mkIns ls = (toLower <$> show funct) <> " " <> intercalate ", " ls
showInstruction ins@(IInstr op rs rt im)
  | op `elem` [Lui] = mkIns [rName rt, dec im]
  | op `elem` [Blez, Bgez] = mkIns [rName rs, dec im]
  | op `elem` [Beq, Bne, Sb] = mkIns [rName rs, rName rt, dec im]
  | op `elem` [Lw, Sw, Lb, Lbu] =
    mkIns [rName rt, dec im <> "(" <> rName rs <> ")"]
  | op `elem` [Lwc1, Ldc1, Swc1] =
    mkIns [fName rt, dec im <> "(" <> rName rs <> ")"]
  | otherwise = mkIns [rName rt, rName rs, dec im]
  where
    mkIns ls = (toLower <$> show op) <> " " <> intercalate ", " ls
    dec = show . BV.int
showInstruction ins@(JInstr op tgt) =
  (toLower <$> show op) <> " " <> printf "0x%08x" (BV.int tgt)
showInstruction ins@(FRInstr funct fmt ft fs fd)
  | funct `elem` [Mfc1, Mtc1] = mkIns [rName ft, fName fs]
  | funct `elem` [Mov, CvtD, CvtS, CvtW] = mkInsWithFormat [fName fd, fName fs]
  | funct `elem` [CLt] = mkInsWithFormat [fName fs, fName ft]
  | funct `elem` [FAdd, FSub, FMul, FDiv] =
    mkAliasedInsWithFormat [fName fd, fName fs, fName ft]
  where
    mkIns ls = (toLower <$> show funct) <> " " <> intercalate ", " ls
    mkInsWithFormat ls =
      insWithFormatName <>
      ['.', toLower (head $ show fmt)] <> " " <> intercalate ", " ls
    mkAliasedInsWithFormat ls =
      drop 2 insWithFormatName <>
      ['.', toLower (head $ show fmt)] <> " " <> intercalate ", " ls
    insWithFormatName =
      toLower <$>
      intercalate
        "."
        (tail $ split (keepDelimsL $ whenElt isUpper) $ show funct)
showInstruction ins@(FIInstr iop ft imm) =
  toLower <$> show iop <> " " <> show (BV.int imm)
showInstruction Syscall = "syscall"
showInstruction Nop = "nop"
showInstruction Break = "break"
showInstruction _ = "XXXXXXXXXXXXXXXXXX"

printStats :: UTCTime -> Computer -> IO ()
printStats startTime c = do
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
      st = view stats c
  putStrLn
    "\x1b[32m\nExecution finished successfully\n--------------------------\n"
  print $ st ^. insCounter
  putStr "Simulation Time: "
  print duration
  putStr "Average IPS: "
  print $
    realToFrac (sumInstructionCounters $ st ^. insCounter) / realToFrac duration
  putStrLn "Simulated execution times for:\n--------------------------"
  putStrLn "Monocycle"
  printEstimatedExecTime st nCycles freqMono
  where
    freq = 33.8688
    freqMono = freq / 4

printEstimatedExecTime :: Stats -> Lens' Stats Int -> Float -> IO ()
printEstimatedExecTime st lens freq = do
  putStrLn $ "\tCycles: " <> show cyc
  putStrLn $ "\tFrequency: " <> printf "%.4f" freq <> " MHz"
  putStrLn $ "\tEstimated execution time: " <> printf "%.4f" seconds <> " sec."
  putStrLn $ "\tIPC: " <> printf "%.2f" ipc
  putStrLn $ "\tMIPS: " <> printf "%.2f" mips
  where
    cyc = st ^. lens
    seconds = fromIntegral cyc / (freq * 10 ^ 6)
    ins = sumInstructionCounters (st ^. insCounter)
    ipc :: Float
    ipc = fromIntegral ins / fromIntegral cyc
    mips = fromIntegral ins / (seconds * 10 ^ 6)

printTrace :: MemoryTrace -> String
printTrace (tp, address, line) =
  unwords [short, hex address, "(line# " <> hex line <> ")"]
  where
    short = [head $ show tp]
    hex = printf "0x%08x"
