module Lib.Run where

import           Lib.Computer

import           Lib.Decode
import qualified Lib.Memory               as M
import           Lib.Operation
import           Lib.Print
import qualified Lib.Registers            as R

import           Control.Monad.State.Lazy (runState)
import           System.IO

runComputer :: Computer -> IO ()
runComputer c0 = do
  let (sc, c1) = tick c0
  mi <- runSyscall sc
  let c2 =
        case mi of
          Just i  -> setInput c1 i
          Nothing -> c1
  if sc == Die
    then putStrLn "--- Program Finished ---"
    else runComputer c2 -- (printComputer c2)

setInput :: Computer -> Int -> Computer
setInput (r, m) i = (R.set 2 (toEnum i) r, m)

runSyscall :: SC -> IO (Maybe Int)
runSyscall GetInt = Just . read <$> getLine
runSyscall sc = do
  runIO sc
  return Nothing
  where
    runIO (PutInt x) = print x
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
