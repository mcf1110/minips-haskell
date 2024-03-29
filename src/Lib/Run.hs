module Lib.Run where

import           Lib.Computer.Types       (Computer, memTrace, stats)
import           Lib.Decode
import qualified Lib.Memory               as M
import           Lib.Operation
import           Lib.Print
import qualified Lib.Registers            as R

import           Control.Monad            (when)
import           Control.Monad.State.Lazy (runState)
import           Data.Maybe               (fromJust, isJust)
import           Data.Time                (NominalDiffTime)
import qualified Data.Word                as W
import           Optics                   (set, (%), (^.))
import           System.IO
import           Text.Printf              (printf)

data SyscallInput
  = GotInt Int
  | GotFloat Float
  | GotDouble Double
  | GotNothing

runComputer :: Maybe Handle -> NominalDiffTime -> Computer -> IO Computer
runComputer file startTime = runWithBreakpoints file startTime []

runWithBreakpoints ::
     Maybe Handle -> NominalDiffTime -> [W.Word32] -> Computer -> IO Computer
runWithBreakpoints file startTime bps c0 = do
  let (sc, c1) = tick c0
  syscallInput <- runSyscall sc
  let c2 = storeInput syscallInput c1
      pc = R.get 32 c2
  c3 <- dumpTrace file c2
  when (pc `elem` bps) $ do
    putStrLn $ "pc: " <> printf "0x%08x" pc
    printComputer c3
    getLine
    putStrLn "---------"
  if sc /= Die
    then runWithBreakpoints file startTime bps c3
    else do
      printStats startTime c3
      return c3

dumpTrace :: Maybe Handle -> Computer -> IO Computer
dumpTrace Nothing c = return c
dumpTrace (Just f) c = do
  let trace = reverse <$> c ^. (stats % memTrace)
  if isJust trace
    then do
      hPutStr f $ unlines $ printTrace <$> fromJust trace
      return $ set (stats % memTrace) (Just []) c
    else do
      return c

storeInput :: SyscallInput -> Computer -> Computer
storeInput syscallInput c =
  case syscallInput of
    GotInt i    -> gotInt c i
    GotFloat f  -> gotFloat c f
    GotDouble d -> gotDouble c d
    GotNothing  -> c

gotInt :: Computer -> Int -> Computer
gotInt comp i = R.set 2 (toEnum i) comp

gotFloat :: Computer -> Float -> Computer
gotFloat comp f = R.setF 0 f comp

gotDouble :: Computer -> Double -> Computer
gotDouble comp d = R.setD 0 d comp

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
tick c0 = runInstruction (decodeInstruction ins) c1
  where
    pc = R.get 32 c0
    (ins, c1) = runState (M.getInstruction pc) c0

runInstruction :: Instr -> Computer -> (SC, Computer)
runInstruction i = runState (evalInstruction i)
