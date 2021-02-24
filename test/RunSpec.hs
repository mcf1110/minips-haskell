module RunSpec
  ( runningTests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib.Computer
import           Lib.Decode
import qualified Lib.Memory       as M
import           Lib.Operation
import qualified Lib.Registers    as R
import           Lib.Run
import           Lib.Segment

testComputerUntilSyscall :: Computer -> (SC, Computer)
testComputerUntilSyscall c =
  case tick c of
    (NoOp, (r, m)) ->
      if M.get (R.get 32 r) m == 0
        then (NoOp, (r, m))
        else testComputerUntilSyscall (r, m)
    x -> x

runSeg :: Segment -> Segment -> Computer
runSeg dataS textS =
  snd $ testComputerUntilSyscall (initialComputer dataS textS)

runSegInitial :: Segment -> Computer
runSegInitial = runSeg []

getSC :: Segment -> Segment -> SC
getSC dataS textS = fst $ runInstruction Syscall $ runSeg dataS textS

regAt ix = R.get ix . fst

runningTests =
  [ testGroup "R Instructions" rTests
  , testGroup "I Instructions" iTests
  , testGroup "J Instructions" jTests
  ]

iTests =
  [ testCase "Cannot overwrite $zero" $
    assertEqual "" 0 (regAt 0 $ runSegInitial [0x24000003])
  , testCase "addi $t0, $zero, 3" $
    assertEqual "" 3 (regAt 8 $ runSegInitial [0x20080003])
  , testCase "addi $t1, $zero, 4" $
    assertEqual "" 4 (regAt 9 $ runSegInitial [0x20090004])
  , testCase "addiu $sp, $sp, -32" $
    assertEqual "" 0x7fffefdc (regAt 29 $ runSegInitial [0x27bdffe0])
  , testCase "add 3 4" $
    assertEqual
      ""
      7
      (regAt 16 $ runSegInitial [0x20080003, 0x20090004, 0x01098020])
  , testCase "lui $a0, 0x1001" $
    assertEqual "" 0x10010000 (regAt 1 $ runSegInitial [0x3c011001])
  , testCase "ori $a0, $at, 0" $
    assertEqual "" 0x10010000 (regAt 4 $ runSegInitial [0x3c011001, 0x34240000])
  , testCase "ori $t1, $t0, 16" $
    assertEqual "" 0x10 (regAt 9 $ runSegInitial [0x24080014, 0x31090010])
  , testCase "beq $zero, $zero, 1" $
    assertEqual
      ""
      (0x00400000 + (2 * 4))
      (regAt 32 $ runSegInitial [0x10000001])
  , testCase "beq $zero, $zero, 1" $
    assertEqual
      ""
      0x0
      (regAt 4 $ runSegInitial [0x10000001, 0x3c011001, 0x34240000])
  , testCase "bne $zero, $zero, 1" $
    assertEqual
      ""
      0x10010000
      (regAt 4 $ runSegInitial [0x15310001, 0x3c011001, 0x34240000])
  , testCase "lw $t0, ans" $
    assertEqual "" 0x2a (regAt 8 $ runSeg [0x2a] [0x3c011001, 0x8c280000])
  , testCase "lw $t1, 4($t0)" $
    assertEqual
      ""
      0x30303234
      (regAt 9 $
       runSeg [0x70736552, 0x30303234] [0x3c011001, 0x34280000, 0x8d090004])
  , testCase "sw $t0, ans" $
    assertEqual
      ""
      0x2a
      (regAt 8 $ runSegInitial [0x2408002a, 0x3c011001, 0xac280000])
  ]

rTests =
  [ testCase "jr $ra" $
    assertEqual
      ""
      (0x00400000 + 4)
      (regAt 32 $ runSegInitial [0x0c100002, 0, 0x03e00008])
  , testCase "slt $t2, 10, 10" $
    assertEqual
      ""
      0
      (regAt 10 $ runSegInitial [0x2408000a, 0x2409000a, 0x0109502a])
  , testCase "slt $t2, 2, 10" $
    assertEqual
      ""
      1
      (regAt 10 $ runSegInitial [0x24080002, 0x2409000a, 0x0109502a])
  , testCase "slt $t3, 0xffffffff, 0" $
    assertEqual
      ""
      1
      (regAt 11 $ runSegInitial [0x24080000, 0x2409ffff, 0x0128582a])
  , testCase "srl $t1, $t0, 2" $
    assertEqual "" 4 (regAt 9 $ runSegInitial [0x24080010, 0x00084882])
  , testCase "sll $t1, $t0, 2" $
    assertEqual "" 0x40 (regAt 9 $ runSegInitial [0x24080010, 0x00084880])
  ]

jTests =
  [ testCase "j 0x40000008" $
    assertEqual
      ""
      0
      (regAt 4 $ runSegInitial [0x08100002, 0x3c011001, 0x34240000])
  , testCase "jal 0x00400008" $
    assertEqual "" (0x00400000 + 8) (regAt 32 $ runSegInitial [0x0c100002])
  , testCase "jal 0x00400008" $
    assertEqual "" (0x00400000 + 4) (regAt 31 $ runSegInitial [0x0c100002])
  ]

systemCallTests =
  [ testCase "print 'Ola mundo!'" $
    assertEqual
      ""
      (PutStr "Ola mundo!")
      (getSC
         [0x20616c4f, 0x646e756d, 8559]
         [0x3c011001, 0x34240000, 0x24020004, 0xc])
  , testCase "getint" $ assertEqual "" GetInt (getSC [] [0x24020005, 0xc])
  , testCase "putInt (3+4)" $
    assertEqual
      ""
      (PutInt 7)
      (getSC [] [0x20080003, 0x20090004, 0x01098020, 0x20020001, 0x102020])
  , testCase "die" $ assertEqual "" Die (getSC [] [0x2002000a])
  , testCase "Unaligned putStr is allowed" $
    assertEqual
      ""
      (PutStr "Voc\195\170 digitou: ")
      (getSC
         [0, 0, 0x5600203a, 0xaac3636f, 0x67696420, 0x756f7469, 0x0000203a]
         [0x3c011001, 0x3424000b, 0x24020004, 0xc])
  ]
