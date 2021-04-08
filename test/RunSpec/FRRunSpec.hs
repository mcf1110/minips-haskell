module RunSpec.FRRunSpec
  ( frTests
  ) where

import           Lib.Computer     (Computer)
import           RunSpec.Helpers  (doubleAt, floatAt, regAt, runSeg,
                                   runSegInitial)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

frTests :: [TestTree]
frTests =
  [ testGroup
      "Move Word From FP"
      [ testCase "Can get PI from coprocessor" $
        assertEqual
          ""
          0x40490fdb
          (regAt 8 $ runSeg [0x40490fdb] [0x3c011001, 0xc4200000, 0x44080000])
      ]
  , testGroup
      "Move Float"
      [ testCase "Can move pi from $f0 to $f1" $
        assertEqual
          ""
          pi
          (floatAt 1 $ runSeg [0x40490fdb] [0x3c011001, 0xc4200000, 0x46000046])
      ]
  , testGroup
      "Move Double"
      [ testCase "Can move pi from $f0 to $f2" $
        assertEqual
          ""
          pi
          (doubleAt 2 $
           runSeg [0x54442d18, 0x400921fb] [0x3c011001, 0xd4200000, 0x46200086])
      ]
  , testGroup
      "Move Word To FP"
      [ testCase "Can move 42 from $t0 to $f0" $
        assertEqual
          ""
          5.9E-44
          (floatAt 0 $ runSegInitial [0x2408002a, 0x44880000])
      ]
  ]
