module RunSpec.FIRunSpec
  ( fiTests
  ) where

import           Lib.Computer.Types (Computer)
import           RunSpec.Helpers    (doubleAt, flagAt, floatAt, regAt, runSeg,
                                     runSegInitial, tc)
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (assertBool, assertEqual, testCase)

fiTests :: [TestTree]
fiTests =
  [ testGroup
      "Branch on FP True"
      [ testGroup
          "Positive"
          [ testCase "BDS" $ assertEqual "" 1 (regAt 8 btPosComputer)
          , testCase "Jumps Accordingly" $
            assertEqual "" 0 (regAt 9 btPosComputer)
          , testCase "Continues running" $
            assertEqual "" 22 (regAt 10 btPosComputer)
          , testCase "Sets PC" $
            assertEqual "" 0x0040001c (regAt 32 btPosComputer)
          ]
      , testGroup
          "Negative"
          [ testCase "BDS" $ assertEqual "" 1 (regAt 8 btNegComputer)
          , testCase "Jumps Accordingly" $
            assertEqual "" 0 (regAt 9 btNegComputer)
          , testCase "Continues running" $
            assertEqual "" 22 (regAt 10 btNegComputer)
          , testCase "Continues running" $
            assertEqual "" 33 (regAt 11 btNegComputer)
          , testCase "Sets PC" $
            assertEqual "" 0x0040002c (regAt 32 btNegComputer)
          ]
      ]
  , testGroup
      "Branch on FP False"
      [ testGroup
          "Positive"
          [ testCase "BDS" $ assertEqual "" 1 (regAt 8 bfPosComputer)
          , testCase "Jumps Accordingly" $
            assertEqual "" 0 (regAt 9 bfPosComputer)
          , testCase "Continues running" $
            assertEqual "" 22 (regAt 10 bfPosComputer)
          , testCase "Sets PC" $
            assertEqual "" 0x00400010 (regAt 32 bfPosComputer)
          ]
      , testGroup
          "Negative"
          [ testCase "BDS" $ assertEqual "" 1 (regAt 8 bfNegComputer)
          , testCase "Jumps Accordingly" $
            assertEqual "" 0 (regAt 9 bfNegComputer)
          , testCase "Continues running" $
            assertEqual "" 22 (regAt 10 bfNegComputer)
          , testCase "Continues running" $
            assertEqual "" 33 (regAt 11 bfNegComputer)
          , testCase "Sets PC" $
            assertEqual "" 0x00400020 (regAt 32 bfNegComputer)
          ]
      ]
  ]

float42 = 0x42280000

btPosComputer =
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

btNegComputer =
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

bfPosComputer =
  runSeg [float42] [0x45000002, 0x24080001, 0x2409000b, 0x240a0016]

bfNegComputer =
  runSegInitial
    [ 0x08100004
    , 0
    , 0x240a0016
    , 0x08100007
    , 0x4500fffd
    , 0x24080001
    , 0x2409000b
    , 0x240b0021
    ]
