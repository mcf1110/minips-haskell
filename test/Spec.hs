import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.BitVector   as BV

import           Lib.Decode
import qualified Lib.Memory       as M
import           Lib.Print
import qualified Lib.Registers    as R

import           RunSpec
import           RunSpec.Helpers  (tc)

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
b :: Integral a => Int -> a -> BV.BitVector
b = BV.bitVec

r :: (Integral a) => Funct -> a -> a -> a -> a -> Instr
r f s t d a = RInstr f (b 5 s) (b 5 t) (b 5 d) (b 5 a)

j :: Integral a => JOp -> a -> Instr
j o t = JInstr o (b 26 t)

i :: (Integral a) => IOp -> a -> a -> a -> Instr
i o s t im = IInstr o (b 5 s) (b 5 t) (b 16 im)

fr :: FFunct -> FFmt -> BV.BV -> BV.BV -> BV.BV -> Instr
fr f fmt t d a = FRInstr f fmt (b 5 t) (b 5 d) (b 6 a)

fi :: Integral a => FIOp -> a -> a -> Instr
fi o s im = FIInstr o (b 5 s) (b 16 im)

rTests =
  [ ( "Add"
    , [ (0x01098020, "add $s0, $t0, $t1", r Add 8 9 16 0)
      , (0x00102020, "add $a0, $zero, $s0", r Add 0 16 4 0)
      ])
  , ( "Add Unsigned"
    , [ (0x00441021, "addu $v0, $v0, $a0", r Addu 2 4 2 0)
      , (0x00641821, "addu $v1, $v1, $a0", r Addu 3 4 3 0)
      ])
  , ("Subtract Unsigned", [(0x01285023, "subu $t2, $t1, $t0", r Subu 9 8 10 0)])
  , ("Jump Register", [(0x03e00008, "jr $ra", r Jr 31 0 0 0)])
  , ("Set Less Than", [(0x0150582a, "slt $t3, $t2, $s0", r Slt 10 16 11 0)])
  , ( "Set Less Than Unsigned"
    , [(0x0109502b, "sltu $t2, $t0, $t1", r Sltu 8 9 10 0)])
  , ("Shift Right Logical", [(0x00041fc2, "srl $v1, $a0, 31", r Srl 0 4 3 31)])
  , ( "Shift Right Arithmetic"
    , [(0x00063403, "sra $a2, $a2, 16", r Sra 0 6 6 16)])
  , ("Shift Left Logical", [(0x00041040, "sll $v0, $a0, 1", r Sll 0 4 2 1)])
  , ("Jump and Link Register", [(0x01008009, "jalr $s0, $t0", r Jalr 8 0 16 0)])
  , ("Mult", [(0x00840018, "mult $a0, $a0", r Mult 4 4 0 0)])
  , ("Move From Low", [(0x00001012, "mflo $v0", r Mflo 0 0 2 0)])
  , ("Move From High", [(0x00001010, "mfhi $v0", r Mfhi 0 0 2 0)])
  , ("Div", [(0x0082001a, "div $a0, $v0", r Div 4 2 0 0)])
  , ("Xor", [(0x00662826, "xor $a1, $v1, $a2", r Xor 3 6 5 0)])
  , ( "And"
    , [ (0x00834024, "and $t0, $a0, $v1", r And 4 3 8 0)
      , (0x00e43824, "and $a3, $a3, $a0", r And 7 4 7 0)
      , (0x00c43024, "and $a2, $a2, $a0", r And 6 4 6 0)
      , (0x00a42824, "and $a1, $a1, $a0", r And 5 4 5 0)
      ])
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
  , ( "Branch on Less Than or Equal to Zero"
    , [(0x1b200017, "blez $t9, 23", i Blez 25 0 23)])
  , ( "Branch on Greater Than or Equal to Zero"
    , [(0x04410007, "bgez $v0, 7", i Bgez 2 1 7)])
  , ( "Load Upper Immediate"
    , [ (0x3c011001, "lui $at, 4097", i Lui 0 1 4097)
      , (0x3c068000, "lui $a2, -32768", i Lui 0 6 (tc 32768))
      ])
  , ("Or Immediate", [(0x34240000, "ori $a0, $at, 0", i Ori 1 4 0)])
  , ( "Load Word"
    , [ (0x8c290000, "lw $t1, 0($at)", i Lw 1 9 0)
      , (0x8d840000, "lw $a0, 0($t4)", i Lw 12 4 0)
      , (0x8d8d0000, "lw $t5, 0($t4)", i Lw 12 13 0)
      , (0x8d090004, "lw $t1, 4($t0)", i Lw 8 9 4)
      ])
  , ("Load Byte", [(0x8184000c, "lb $a0, 12($t4)", i Lb 12 4 12)])
  , ("Load Byte Unsigned", [(0x93c30008, "lbu $v1, 8($fp)", i Lbu 30 3 8)])
  , ("Store Word", [(0xad8d0000, "sw $t5, 0($t4)", i Sw 12 13 0)])
  , ( "Set on Less Than Immediate"
    , [(0x28880009, "slti $t0, $a0, 9", i Slti 4 8 9)])
  , ( "Load Word To Floating Point"
    , [ (0xc42c0000, "lwc1 $f12, 0($at)", i Lwc1 1 12 0)
      , (0xc4440048, "lwc1 $f4, 72($v0)", i Lwc1 2 4 72)
      ])
  , ( "Load Double Word To Floating Point"
    , [ (0xd42c0008, "ldc1 $f12, 8($at)", i Ldc1 1 12 8)
      , (0xd4200000, "ldc1 $f0, 0($at)", i Ldc1 1 0 0)
      ])
  , ( "Store Word From Floating Point"
    , [(0xe7cc0008, "swc1 $f12, 8($fp)", i Swc1 30 12 8)])
  , ("Store Byte", [(0xa3c20008, "sb $fp, $v0, 8", i Sb 30 2 8)])
  ]

frTests =
  [ ( "Move Word From Floating Point"
    , [(0x44036000, "mfc1 $v1, $f12", fr Mfc1 Single 3 12 0)])
  , ( "Move Word To Floating Point"
    , [(0x44830000, "mtc1 $v1, $f0", fr Mtc1 Single 3 0 0)])
  , ( "Floating Point Move"
    , [ (0x46000306, "mov.s $f12, $f0", fr Mov Single 0 0 12)
      , (0x46200306, "mov.d $f12, $f0", fr Mov Double 0 0 12)
      ])
  , ( "Convert to Double"
    , [ (0x468010a1, "cvt.d.w $f2, $f2", fr CvtD Word 0 2 2)
      , (0x460010a1, "cvt.d.s $f2, $f2", fr CvtD Single 0 2 2)
      ])
  , ( "Convert to Single"
    , [ (0x468000a0, "cvt.s.w $f2, $f0", fr CvtS Word 0 0 2)
      , (0x46200020, "cvt.s.d $f0, $f0", fr CvtS Double 0 0 0)
      ])
  , ( "Convert to Word"
    , [ (0x460000a4, "cvt.w.s $f2, $f0", fr CvtW Single 0 0 2)
      , (0x462000a4, "cvt.w.d $f2, $f0", fr CvtW Double 0 0 2)
      ])
  , ( "FP Add"
    , [ (0x46221080, "add.d $f2, $f2, $f2", fr FAdd Double 2 2 2)
      , (0x46020000, "add.s $f0, $f0, $f2", fr FAdd Single 2 0 0)
      ])
  , ( "FP Sub"
    , [ (0x46262101, "sub.d $f4, $f4, $f6", fr FSub Double 6 4 4)
      , (0x46062101, "sub.s $f4, $f4, $f6", fr FSub Single 6 4 4)
      ])
  , ( "FP Div"
    , [ (0x46222083, "div.d $f2, $f4, $f2", fr FDiv Double 2 4 2)
      , (0x46022083, "div.s $f2, $f4, $f2", fr FDiv Single 2 4 2)
      ])
  , ( "FP Mult"
    , [ (0x46220002, "mul.d $f0, $f0, $f2", fr FMul Double 2 0 0)
      , (0x46020002, "mul.s $f0, $f0, $f2", fr FMul Single 2 0 0)
      ])
  , ( "FP Compare"
    , [ (0x460e403c, "c.lt.s $f8, $f14", fr CLt Single 14 8 0)
      , (0x4610003c, "c.lt.s $f0, $f16", fr CLt Single 16 0 0)
      ])
  ]

fiTests =
  [ ( "Branch on FP True"
    , [ (0x4501fffe, "bc1t -2", fi Bc1t 8 (0xffff - 2 + 1))
      , (0x4501fff2, "bc1t -14", fi Bc1t 8 (0xffff - 14 + 1))
      , (0x45010001, "bc1t 1", fi Bc1t 8 1)
      , (0x4501fffa, "bc1t -6", fi Bc1t 8 (0xffff - 6 + 1))
      ])
  , ( "Branch on FP False"
    , [ (0x4500fffe, "bc1f -2", fi Bc1f 8 (0xffff - 2 + 1))
      , (0x4500fff2, "bc1f -14", fi Bc1f 8 (0xffff - 14 + 1))
      , (0x45000001, "bc1f 1", fi Bc1f 8 1)
      ])
  ]

decodingTests =
  [ testGroup "R Instructions" $ map makeDecTest rTests
  , testGroup "J Instructions" $ map makeDecTest iTests
  , testGroup "I Instructions" $ map makeDecTest jTests
  , testGroup "FR Instructions" $ map makeDecTest frTests
  , testGroup "FI Instructions" $ map makeDecTest fiTests
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
  , testGroup "FI Instructions" $ map makePrintTest fiTests
  , testCase "Syscall" $ assertEqual "" "syscall" (showInstruction Syscall)
  , testCase "Nop" $ assertEqual "" "nop" (showInstruction Nop)
  , testCase "Break" $ assertEqual "" "break" (showInstruction Break)
  ]
  where
    makePrintTest (title, ts) = testGroup title $ map dt ts
    dt (_, src, instr) =
      testCase src $ assertEqual "" src (showInstruction instr)
