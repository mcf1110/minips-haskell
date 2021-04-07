module Lib.Run where

import           Lib.Computer
import           Lib.Decode
import qualified Lib.Memory               as M
import           Lib.Operation
import           Lib.Print
import qualified Lib.Registers            as R

import           Control.Monad            (when)
import           Control.Monad.State.Lazy (runState)
import           System.IO
import           Text.Printf              (printf)

import qualified Data.Word                as W

data SyscallInput
  = GotInt Int
  | GotFloat Float
  | GotDouble Double
  | GotNothing

runComputer :: Computer -> IO ()
runComputer = runWithBreakpoints []

runWithBreakpoints :: [W.Word32] -> Computer -> IO ()
runWithBreakpoints bps c0 = do
  let (sc, c1) = tick c0
  mi <- runSyscall sc
  let c2 =
        case mi of
          GotInt i    -> gotInt c1 i
          GotFloat f  -> gotFloat c1 f
          GotDouble d -> gotDouble c1 d
          GotNothing  -> c1
  let pc = R.get 32 $ fst c2
  when (pc `elem` bps) $ do
    putStrLn $ "pc: " <> printf "0x%08x" pc
    printComputer c2
    getLine
    putStrLn "---------"
  if sc == Die
    then putStrLn "\n--- Program Finished ---"
    else runWithBreakpoints bps c2

gotInt :: Computer -> Int -> Computer
gotInt (r, m) i = (R.set 2 (toEnum i) r, m)

gotFloat :: Computer -> Float -> Computer
gotFloat (r, m) f = (R.setF 0 f r, m)

gotDouble :: Computer -> Double -> Computer
gotDouble (r, m) d = (R.setD 0 d r, m)

runSyscall :: SC -> IO SyscallInput
runSyscall GetInt = GotInt . read <$> getLine
runSyscall GetFloat = GotFloat . read <$> getLine
runSyscall GetDouble = GotDouble . read <$> getLine
runSyscall sc = do
  runIO sc
  return GotNothing
  where
    runIO (PutInt x) = putStr $ show x
    runIO (PutFloat x) = putStr $ show x
    runIO (PutDouble x) = putStr $ show x
    runIO (PutStr x) = do
      putStr x
      hFlush stdout
    runIO (PutChar x) = putChar x
    runIO _ = return ()

tick :: Computer -> (SC, Computer)
tick c0 = runInstruction ins c0
  where
    (r, m) = c0
    pc = R.get 32 r
    ins = decodeInstruction $ M.get pc m

runInstruction :: Instr -> Computer -> (SC, Computer)
runInstruction i = runState (evalInstruction i)
