module RunSpec.Helpers where

import qualified Data.Word     as W
import           Lib.Computer  (Computer, initialComputer)
import           Lib.Decode    (Instr (Syscall))
import qualified Lib.Memory    as M
import           Lib.Operation (SC (NoSC))
import qualified Lib.Registers as R
import           Lib.Run       (runInstruction, tick)
import           Lib.Segment   (Segment)

tc :: Num a => a -> a
tc x = 0xffffffff - x + 1 -- twos complement

testComputerUntilSyscall :: Computer -> (SC, Computer)
testComputerUntilSyscall c =
  case tick c of
    (NoSC, (r, m)) ->
      if M.get (R.get 32 r) m == 0
        then (NoSC, (r, m))
        else testComputerUntilSyscall (r, m)
    x -> x

runSeg :: Segment -> Segment -> Computer
runSeg dataS textS =
  snd $ testComputerUntilSyscall (initialComputer dataS textS [])

runSegInitial :: Segment -> Computer
runSegInitial = runSeg []

getSC :: Segment -> Segment -> SC
getSC dataS textS = fst $ runInstruction Syscall $ runSeg dataS textS

regAt :: Enum a => a -> (R.Registers, b) -> W.Word32
regAt ix = R.get ix . fst

floatAt :: Enum a => a -> (R.Registers, b) -> Float
floatAt ix = R.getF ix . fst

doubleAt :: (Num a, Enum a) => a -> (R.Registers, b) -> Double
doubleAt ix = R.getD ix . fst
