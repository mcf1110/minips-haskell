import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.BitVector   as BV

import           Lib.Computer
import           Lib.Decode
import           Lib.Operation
import           Lib.Print
import qualified Lib.Registers    as R
import           Lib.Run
import           Lib.Segment

main :: IO ()
main = defaultMain tests

tests =
  testGroup
    "Unit tests"
    [ testCase "wordToBV" $
      assertEqual "" (wordToBV 0x00af8020) (BV.fromBits bits)
    , testGroup "Decoding" decodingTests
    , testGroup "Printing" printingTests
    , testGroup "Running" runningTests
    ]
  where
    bits =
      replicate 8 False <>
      [True, False, True, False] <>
      replicate 5 True <> replicate 9 False <> [True] <> replicate 5 False

-- helpers
b = BV.bitVec

r f s t d a = RInstr f (b 5 s) (b 5 t) (b 5 d) (b 6 a)

j o t = JInstr o (b 26 t)

i o s t im = IInstr o (b 5 s) (b 5 t) (b 16 im)

rTests =
  [ ( "Add"
    , [ (0x01098020, "add $s0, $t0, $t1", r Add 8 9 16 0)
      , (0x00102020, "add $a0, $zero, $s0", r Add 0 16 4 0)
      ])
  , ( "Add Unsigned"
    , [ (0x00441021, "addu $v0, $v0, $a0", r Addu 2 4 2 0)
      , (0x00641821, "addu $v1, $v1, $a0", r Addu 3 4 3 0)
      ])
  , ("Jump Register", [(0x03e00008, "jr $ra", r Jr 31 0 0 0)])
  , ("Set Less Than", [(0x0150582a, "slt $t3, $t2, $s0", r Slt 10 16 11 0)])
  , ("Shift Right Logical", [(0x00041fc2, "srl $v1, $a0, 31", r Srl 0 4 3 31)])
  , ("Shift Left Logical", [(0x00041040, "sll $v0, $a0, 1", r Sll 0 4 2 1)])
  ]

jTests =
  [ ( "Jump"
    , [ (0x08100022, "j 0x00400088", j J 0x00400088)
      , (0x08100026, "j 0x00400098", j J 0x00400098)
      ])
  , ("Jump and Link", [(0x0c10001d, "jal 0x00400074", j Jal 0x00400074)])
  ]

iTests =
  [ ( "Add Immediate"
    , [ (0x20080003, "addi $t0, $zero, 3", i Addi 0 8 3)
      , (0x20090004, "addi $t1, $zero, 4", i Addi 0 9 4)
      , (0x20020001, "addi $v0, $zero, 1", i Addi 0 2 1)
      , (0x2002000a, "addi $v0, $zero, 10", i Addi 0 2 10)
      ])
  , ( "Add Immediate Unsigned"
    , [ (0x24020004, "addiu $v0, $zero, 4", i Addiu 0 2 4)
      , (0x2404000a, "addiu $a0, $zero, 10", i Addiu 0 4 10)
      , (0x2402000b, "addiu $v0, $zero, 11", i Addiu 0 2 11)
      ])
  , ("And Immediate", [(0x30860001, "andi $a2, $a0, 1", i Andi 4 6 1)])
  , ( "Branch on Equal"
    , [ (0x10000006, "beq $zero, $zero, 6", i Beq 0 0 6)
      , (0x11310012, "beq $t1, $s1, 18", i Beq 9 17 18)
      , (0x12200002, "beq $s1, $zero, 2", i Beq 17 0 2)
      ])
  , ( "Branch on Not Equal"
    , [ (0x1531000e, "bne $t1, $s1, 14", i Bne 9 17 14)
      , (0x15310005, "bne $t1, $s1, 5", i Bne 9 17 5)
      , (0x1487fff4, "bne $a0, $a3, -12", i Bne 4 7 $ BV.bitVec 16 (-12))
      ])
  , ( "Load Upper Immediate"
    , [(0x3c011001, "lui $at, 0x00001001", i Lui 0 1 4097)])
  , ("Or Immediate", [(0x34240000, "ori $a0, $at, 0", i Ori 1 4 0)])
  , ( "Load Word"
    , [ (0x8c290000, "lw $t1, 0($at)", i Lw 1 9 0)
      , (0x8d840000, "lw $a0, 0($t4)", i Lw 12 4 0)
      , (0x8d8d0000, "lw $t5, 0($t4)", i Lw 12 13 0)
      ])
  , ("Store Word", [(0xad8d0000, "sw $t5, 0($t4)", i Sw 12 13 0)])
  ]

decodingTests =
  [ testGroup "R Instructions" $ map makeDecTest rTests
  , testGroup "J Instructions" $ map makeDecTest iTests
  , testGroup "I Instructions" $ map makeDecTest jTests
  , testCase "Syscall" $ assertEqual "" Syscall (decode 0x0000000c)
  ]
  where
    makeDecTest (title, ts) = testGroup title $ map dt ts
    dt (code, src, instr) = testCase src $ assertEqual "" instr (decode code)

printingTests =
  [ testGroup "R Instructions" $ map makePrintTest rTests
  , testGroup "J Instructions" $ map makePrintTest iTests
  , testGroup "I Instructions" $ map makePrintTest jTests
  , testCase "Syscall" $ assertEqual "" "syscall" (showInstruction Syscall)
  ]
  where
    makePrintTest (title, ts) = testGroup title $ map dt ts
    dt (_, src, instr) =
      testCase src $ assertEqual "" src (showInstruction instr)

runSegInitial :: Segment -> Computer
runSegInitial segment =
  foldl
    (\s i -> snd $ runInstruction i s)
    (initialComputer [] segment)
    (decodeProgram segment)

getSC :: Segment -> Segment -> SC
getSC dataS textS =
  fst $
  runInstruction Syscall $
  foldl
    (\s i -> snd $ runInstruction i s)
    (initialComputer dataS textS)
    (decodeProgram textS)

regAt ix = R.get ix . fst

runningTests =
  [ testCase "addi $t0, $zero, 3" $
    assertEqual "" 3 (regAt 8 $ runSegInitial [0x20080003])
  , testCase "addi $t1, $zero, 4" $
    assertEqual "" 4 (regAt 9 $ runSegInitial [0x20090004])
  , testCase "3 + 4" $
    assertEqual
      ""
      7
      (regAt 16 $ runSegInitial [0x20080003, 0x20090004, 0x01098020])
  , testCase "putInt (3+4)" $
    assertEqual
      ""
      (PutInt 7)
      (getSC [] [0x20080003, 0x20090004, 0x01098020, 0x20020001, 0x102020])
  , testCase "die" $ assertEqual "" Die (getSC [] [0x2002000a])
  , testCase "lui $a0, 0x1001" $
    assertEqual "" 0x10010000 (regAt 1 $ runSegInitial [0x3c011001])
  , testCase "ori $a0, $at, 0" $
    assertEqual "" 0x10010000 (regAt 4 $ runSegInitial [0x3c011001, 0x34240000])
  , testCase "print 'Ola mundo!'" $
    assertEqual
      ""
      (PutStr "Ola mundo!")
      (getSC
         [0x20616c4f, 0x646e756d, 8559]
         [0x3c011001, 0x34240000, 0x24020004, 0xc])
  , testCase "getint" $ assertEqual "" GetInt (getSC [] [0x24020005, 0xc])
  , testCase "Unaligned putStr is allowed" $
    assertEqual
      ""
      (PutStr "Voc\195\170 digitou: ")
      (getSC
         [0, 0, 0x5600203a, 0xaac3636f, 0x67696420, 0x756f7469, 0x0000203a]
         [0x3c011001, 0x3424000b, 0x24020004, 0xc])
  ]
