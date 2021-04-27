module RunSpec.Helpers where

import qualified Data.Word          as W
import           Lib.Computer       (initialComputer)
import           Lib.Computer.Types
import           Lib.Decode         (Instr (Syscall))
import qualified Lib.Memory         as M
import           Lib.Operation      (SC (NoSC))
import qualified Lib.Registers      as R
import           Lib.Run            (runInstruction, tick)
import           Lib.Segment        (Segment)

tc :: Num a => a -> a
tc x = 0xffffffff - x + 1 -- twos complement

testComputerUntilSyscall :: Computer -> (SC, Computer)
testComputerUntilSyscall c =
  case tick c of
    (NoSC, comp) ->
      if M._getWithoutLatency (R.get 32 comp) comp == 0
        then (NoSC, comp)
        else testComputerUntilSyscall comp
    x -> x

runSeg :: Segment -> Segment -> Computer
runSeg dataS textS =
  snd $ testComputerUntilSyscall (initialComputer dataS textS [])

runSegInitial :: Segment -> Computer
runSegInitial = runSeg []

getSC :: Segment -> Segment -> SC
getSC dataS textS = fst $ runInstruction Syscall $ runSeg dataS textS

regAt :: Enum a => a -> Computer -> W.Word32
regAt = R.get

floatAt :: Enum a => a -> Computer -> Float
floatAt = R.getF

doubleAt :: (Num a, Enum a) => a -> Computer -> Double
doubleAt = R.getD

flagAt :: (Num a, Enum a) => a -> Computer -> Bool
flagAt = R.getFlag

memAt :: Enum a => a -> Computer -> W.Word32
memAt = M._getWithoutLatency
