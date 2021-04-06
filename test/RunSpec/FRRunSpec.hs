module RunSpec.FRRunSpec
  ( frTests
  ) where

import           Lib.Computer     (Computer)
import           RunSpec.Helpers  (floatAt, regAt, runSeg, runSegInitial)
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
  ]
