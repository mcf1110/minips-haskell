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
    (NoSC, (r, m)) ->
      if M.get (R.get 32 r) m == 0
        then (NoSC, (r, m))
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
  , testGroup
      "Add Immediate"
      [ testCase "addi $t0, $zero, 3 - loads 3 to $t0" $
        assertEqual "" 3 (regAt 8 $ runSegInitial [0x20080003])
      , testCase "addi $t1, $zero, 4 - loads 4 to $t1" $
        assertEqual "" 4 (regAt 9 $ runSegInitial [0x20090004])
      ]
  , testGroup
      "Add Immediate Upper"
      [ testCase "addiu $sp, $sp, -32" $
        assertEqual "" 0x7fffefdc (regAt 29 $ runSegInitial [0x27bdffe0])
      ]
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
  , testGroup
      "Branch Delay"
      [ testCase "Branch Delay" $
        assertEqual
          "Executes instruction in BDS"
          1
          (regAt 9 $
           runSegInitial [0x10000002, 0x24090001, 0x240a0002, 0x240b0003])
      , testCase "Does not execute BDS+1" $
        assertEqual
          ""
          0
          (regAt 10 $
           runSegInitial [0x10000002, 0x24090001, 0x240a0002, 0x240b0003])
      , testCase "Branches accordingly" $
        assertEqual
          ""
          3
          (regAt 11 $
           runSegInitial [0x10000002, 0x24090001, 0x240a0002, 0x240b0003])
      ]
  , testGroup
      "Branch on Equal"
      [ testCase "Changes PC correctly" $
        assertEqual
          ""
          (0x00400000 + (2 * 4))
          (regAt 32 $ runSegInitial [0x10000001])
      , testCase "Executes correct instructions" $
        assertEqual
          ""
          0x0
          (regAt 4 $ runSegInitial [0x10000002, 0x0, 0x3c011001, 0x34240000])
      ]
  , testGroup
      "Branch Not Equal - bne 1, $zero, 2"
      [ testCase "BDS" $
        assertEqual
          ""
          15
          (regAt 9 $
           runSegInitial
             [0x24090001, 0x15200002, 0x2409000f, 0x240a0002, 0x240b0003])
      , testCase "BDS+1" $
        assertEqual
          ""
          0
          (regAt 10 $
           runSegInitial
             [0x24090001, 0x15200002, 0x2409000f, 0x240a0002, 0x240b0003])
      , testCase "Branches accordingly" $
        assertEqual
          ""
          3
          (regAt 11 $
           runSegInitial
             [0x24090001, 0x15200002, 0x2409000f, 0x240a0002, 0x240b0003])
      ]
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
  , testGroup
      "slti"
      [ testCase "Sets reg to True" $
        assertEqual "" 1 (regAt 9 $ runSegInitial [0x2408002a, 0x29090032])
      , testCase "Sets reg to False" $
        assertEqual "" 0 (regAt 9 $ runSegInitial [0x2408002a, 0x2909001e])
      , testCase "Deals with negative numbers" $
        assertEqual "" 0 (regAt 9 $ runSegInitial [0x2408fff4, 0x2909ffd6])
      , testCase "Deals with negative numbers pt2" $
        assertEqual "" 1 (regAt 9 $ runSegInitial [0x2408ffd6, 0x2909fff4])
      ]
  ]

jrComputer :: Computer
jrComputer =
  runSegInitial
    [0x3c010040, 0x34290014, 0x01200008, 0x2408002a, 0x240b002a, 0x240a0016]

rTests =
  [ testGroup
      "Jump register"
      [ testCase "Works with jal" $
        assertEqual
          ""
          (0x00400000 + 4)
          (regAt 32 $ runSegInitial [0x0c100002, 0, 0x03e00008])
      , testCase "Changes PC" $
        assertEqual "" (0x00400000 + 6 * 4) (regAt 32 jrComputer)
      , testCase "Jumps correctly" $ assertEqual "" 22 (regAt 10 jrComputer)
      , testCase "BDL" $ assertEqual "" 42 (regAt 8 jrComputer)
      , testCase "Not BDL+1" $ assertEqual "" 0 (regAt 11 jrComputer)
      ]
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
  [ testGroup
      "Jump"
      [ testCase "j 0x40000008" $
        assertEqual
          ""
          0
          (regAt 4 $ runSegInitial [0x08100003, 0, 0x3c011001, 0x34240000])
      , testCase "j 0x40000008" $
        assertEqual
          ""
          0x400010
          (regAt 32 $ runSegInitial [0x08100003, 0, 0x3c011001, 0x34240000])
      , testCase "BDL" $
        assertEqual
          ""
          11
          (regAt 9 $
           runSegInitial [0x08100003, 0x2409000b, 0x240a0016, 0x240b0021])
      , testCase "BDL+1" $
        assertEqual
          ""
          0
          (regAt 10 $
           runSegInitial [0x08100003, 0x2409000b, 0x240a0016, 0x240b0021])
      , testCase "Jumps to instruction" $
        assertEqual
          ""
          33
          (regAt 11 $
           runSegInitial [0x08100003, 0x2409000b, 0x240a0016, 0x240b0021])
      ]
  , testGroup
      "Jump and Link - jal 0x00400008"
      [ testCase "Sets PC" $
        assertEqual "" (0x00400000 + 8) (regAt 32 $ runSegInitial [0x0c100002])
      , testCase "Sets $ra" $
        assertEqual "" (0x00400000 + 4) (regAt 31 $ runSegInitial [0x0c100002])
      ]
  , testGroup
      "Jalr"
      [ testCase "BDS" $ assertEqual "" 42 (regAt 9 jalrComputer)
      , testCase "$t2" $ assertEqual "" 0 (regAt 10 jalrComputer)
      , testCase "$t3" $ assertEqual "" 33 (regAt 11 jalrComputer)
      , testCase "$t4" $ assertEqual "" 44 (regAt 12 jalrComputer)
      ]
  ]

jalrComputer =
  runSegInitial
    [ 0x3c010040 --la      $t0, lbl
    , 0x3428001c
    , 0x01008009 -- jalr    $s0, $t0
    , 0x24090020 -- li $t1, 32
    , 0x240c002c -- li $t4, 44
    , 0x3c010040 -- la $s0, fim
    , 0x34300024
    , 0x21290005 -- addi $t1, $t1, 21
    , 0x02000008 -- jr       $s0
    , 0x240b0021 -- li $t3, 33
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
