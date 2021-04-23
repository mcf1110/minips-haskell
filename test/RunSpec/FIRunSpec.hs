module RunSpec.FIRunSpec
  ( fiTests
  ) where

import           Lib.Computer     (Computer)
import           RunSpec.Helpers  (doubleAt, flagAt, floatAt, regAt, runSeg,
                                   runSegInitial, tc)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, assertEqual, testCase)

fiTests :: [TestTree]
fiTests =
  [ testGroup
      "Branch on FP True"
      [ testGroup
          "Positive"
          [ testCase "BDS" $ assertEqual "" 1 (regAt 8 jumpPositiveComputer)
          , testCase "Jumps Accordingly" $
            assertEqual "" 0 (regAt 9 jumpPositiveComputer)
          , testCase "Continues running" $
            assertEqual "" 22 (regAt 10 jumpPositiveComputer)
          , testCase "Sets PC" $
            assertEqual "" 0x0040001c (regAt 32 jumpPositiveComputer)
          ]
      , testGroup
          "Negative"
          [ testCase "BDS" $ assertEqual "" 1 (regAt 8 jumpNegativeComputer)
          , testCase "Jumps Accordingly" $
            assertEqual "" 0 (regAt 9 jumpNegativeComputer)
          , testCase "Continues running" $
            assertEqual "" 22 (regAt 10 jumpNegativeComputer)
          , testCase "Continues running" $
            assertEqual "" 33 (regAt 11 jumpNegativeComputer)
          , testCase "Sets PC" $
            assertEqual "" 0x0040002c (regAt 32 jumpNegativeComputer)
          ]
      ]
  ]

float42 = 0x42280000

jumpPositiveComputer =
  runSeg
    [float42]
    [ 0x3c011001
    , 0xc4200000
    , 0x4600083c
    , 0x45010002
    , 0x24080001
    , 0x2409000b
    , 0x240a0016
    ]

jumpNegativeComputer =
  runSeg
    [float42]
    [ 0x08100004
    , 0
    , 0x240a0016
    , 0x0810000a
    , 0x3c011001
    , 0xc4200000
    , 0x4600083c
    , 0x4501fffa
    , 0x24080001
    , 0x2409000b
    , 0x240b0021
    ]
