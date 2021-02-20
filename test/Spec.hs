import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.BitVector   as BV
import           Lib.Decode
import           Lib.Print

main :: IO ()
main = defaultMain tests

tests =
  testGroup
    "Unit tests"
    [ testCase "wordToBV" $ assertEqual "" (wordToBV 0x00af8020) (BV.fromBits b)
    , testGroup "Decoding" decodingTests
    , testGroup "Printing" printingTests
    ]
  where
    b =
      replicate 8 False <>
      [True, False, True, False] <>
      replicate 5 True <> replicate 9 False <> [True] <> replicate 5 False

rTests =
  [ ( "Add"
    , [ (0x01098020, "add $s0, $t0, $t1", (RInstr Add 8 9 16 0))
      , (0x00102020, "add $a0, $zero, $s0", (RInstr Add 0 16 4 0))
      ])
  , ( "Add Unsigned"
    , [ (0x00441021, "addu $v0, $v0, $a0", (RInstr Addu 2 4 2 0))
      , (0x00641821, "addu $v1, $v1, $a0", (RInstr Addu 3 4 3 0))
      ])
  , ("Jump Register", [(0x03e00008, "jr $ra", (RInstr Jr 31 0 0 0))])
  , ( "Set Less Than"
    , [(0x0150582a, "slt $t3, $t2, $s0", (RInstr Slt 10 16 11 0))])
  ]

jTests =
  [ ( "Jump"
    , [ (0x08100022, "j 0x00400088", (JInstr J 0x00400088))
      , (0x08100026, "j 0x00400098", (JInstr J 0x00400098))
      ])
  , ("Jump and Link", [(0x0c10001d, "jal 0x00400074", (JInstr Jal 0x00400074))])
  ]

iTests =
  [ ( "Add Immediate"
    , [ (0x20080003, "addi $t0, $zero, 3", (IInstr Addi 0 8 3))
      , (0x20090004, "addi $t1, $zero, 4", (IInstr Addi 0 9 4))
      , (0x20020001, "addi $v0, $zero, 1", (IInstr Addi 0 2 1))
      , (0x2002000a, "addi $v0, $zero, 10", (IInstr Addi 0 2 10))
      ])
  , ( "Add Immediate Unsigned"
    , [ (0x24020004, "addiu $v0, $zero, 4", (IInstr Addiu 0 2 4))
      , (0x2404000a, "addiu $a0, $zero, 10", (IInstr Addiu 0 4 10))
      , (0x2402000b, "addiu $v0, $zero, 11", (IInstr Addiu 0 2 11))
      ])
  , ( "Branch on Equal"
    , [ (0x10000006, "beq $zero, $zero, 0x00000006", (IInstr Beq 0 0 6))
      , (0x11310012, "beq $t1, $s1, 0x00000012", (IInstr Beq 9 17 18))
      , (0x12200002, "beq $s1, $zero, 0x00000002", (IInstr Beq 17 0 2))
      ])
  , ( "Branch on Not Equal"
    , [ (0x1531000e, "bne $t1, $s1, 0x0000000e", (IInstr Bne 9 17 14))
      , (0x15310005, "bne $t1, $s1, 0x00000005", (IInstr Bne 9 17 5))
      ])
  , ( "Load Upper Immediate"
    , [(0x3c011001, "lui $at, 0x00001001", (IInstr Lui 0 1 4097))])
  , ("Or Immediate", [(0x34240000, "ori $a0, $at, 0", (IInstr Ori 1 4 0))])
  ]

decodingTests =
  [ testGroup "R Instructions" $ map makeDecTest rTests
  , testGroup "J Instructions" $ map makeDecTest iTests
  , testGroup "I Instructions" $ map makeDecTest jTests
  , testCase "Syscall" $ assertEqual "" (Syscall) (decode 0x0000000c)
  ]
  where
    makeDecTest (title, ts) = testGroup title $ map dt ts
    dt (code, src, instr) = testCase src $ assertEqual "" instr (decode code)

printingTests =
  [ testGroup "R Instructions" $ map makePrintTest rTests
  , testGroup "J Instructions" $ map makePrintTest iTests
  , testGroup "I Instructions" $ map makePrintTest jTests
  , testCase "Syscall" $ assertEqual "" ("syscall") (showInstruction Syscall)
  ]
  where
    makePrintTest (title, ts) = testGroup title $ map dt ts
    dt (_, src, instr) =
      testCase src $ assertEqual "" src (showInstruction instr)
