import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.BitVector   as BV

import           Lib.Computer
import           Lib.Decode
import qualified Lib.Memory       as M
import           Lib.Operation
import           Lib.Print
import qualified Lib.Registers    as R
import           Lib.Run
import           Lib.Segment

import           RunSpec

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

fr f s t d a = FRInstr f (b 5 s) (b 5 t) (b 5 d) (b 6 a)

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
  , ("Jump and Link Register", [(0x01008009, "jalr $s0, $t0", r Jalr 8 0 16 0)])
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
      , (0x8d090004, "lw $t1, 4($t0)", i Lw 8 9 4)
      ])
  , ("Store Word", [(0xad8d0000, "sw $t5, 0($t4)", i Sw 12 13 0)])
  , ( "Set on Less Than Immediate"
    , [(0x28880009, "slti $t0, $a0, 9", i Slti 4 8 9)])
  ]

frTests =
  [ ( "Move Word From Floating Point"
    , [(0x44036000, "mfc1 $v1, $f12", fr Mfc1 0 3 12 0)])
  , ( "Move Word To Floating Point"
    , [(0x44830000, "mtc1 $v1, $f0", fr Mtc1 4 3 0 0)])
  ]

decodingTests =
  [ testGroup "R Instructions" $ map makeDecTest rTests
  , testGroup "J Instructions" $ map makeDecTest iTests
  , testGroup "I Instructions" $ map makeDecTest jTests
  , testGroup "FR Instructions" $ map makeDecTest frTests
  , testCase "Syscall" $ assertEqual "" Syscall (decode 0x0000000c)
  , testCase "Break" $ assertEqual "" Break (decode 0x0000000d)
  , testCase "Nop" $ assertEqual "" Nop (decode 0x00000000)
  ]
  where
    makeDecTest (title, ts) = testGroup title $ map dt ts
    dt (code, src, instr) = testCase src $ assertEqual "" instr (decode code)

printingTests =
  [ testGroup "R Instructions" $ map makePrintTest rTests
  , testGroup "J Instructions" $ map makePrintTest iTests
  , testGroup "I Instructions" $ map makePrintTest jTests
  , testGroup "FR Instructions" $ map makePrintTest frTests
  , testCase "Syscall" $ assertEqual "" "syscall" (showInstruction Syscall)
  , testCase "Nop" $ assertEqual "" "nop" (showInstruction Nop)
  , testCase "Break" $ assertEqual "" "break" (showInstruction Break)
  ]
  where
    makePrintTest (title, ts) = testGroup title $ map dt ts
    dt (_, src, instr) =
      testCase src $ assertEqual "" src (showInstruction instr)
