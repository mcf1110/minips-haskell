import Test.Tasty
import Test.Tasty.HUnit

import Lib.Decode
import qualified Data.BitVector as BV

main :: IO ()
main = defaultMain tests

tests = testGroup "Unit tests"
  [ testCase "wordToBV" $
      assertEqual "" (wordToBV 0x00af8020) (BV.fromBits b),
      testGroup "Decoding" decodingTests
  ] where b = replicate 8 False <> 
              [True, False, True, False] <> 
              replicate 5 True <>
              replicate 9 False <> [True] <>
              replicate 5 False

decodingTests = [
  testGroup "R Instructions" rTests,
  testGroup "J Instructions" iTests,
  testGroup "I Instructions" jTests,
  testCase "Syscall" $ assertEqual "" (Syscall) (decode 0x0000000c)
  ]



rTests = [testGroup "Add" [ 
            testCase "add $s0, $t0, $t1" $ assertEqual "" (RInstr Add 8 9 16 0) (decode 0x01098020),
            testCase "add $a0, $zero, $s0" $ assertEqual "" (RInstr Add 0 16 4 0) (decode 0x00102020)
            ]
          ]
jTests = []
iTests = [testGroup "Add Immediate" [ 
            testCase "addi $t0, $zero, 3" $ assertEqual "" (IInstr AddI 0 8 3) (decode 0x20080003),
            testCase "addi $t1, $zero, 4" $ assertEqual "" (IInstr AddI 0 9 4) (decode 0x20090004),
            testCase "addi $v0, $zero, 1" $ assertEqual "" (IInstr AddI 0 2 1) (decode 0x20020001),
            testCase "addi $v0, $zero, 10" $ assertEqual "" (IInstr AddI 0 2 10) (decode 0x2002000a)
            ]
          ]
