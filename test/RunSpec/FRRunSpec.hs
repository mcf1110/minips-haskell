module RunSpec.FRRunSpec
  ( frTests
  ) where

import           Lib.Computer     (Computer)
import           RunSpec.Helpers  (doubleAt, floatAt, regAt, runSeg,
                                   runSegInitial, tc)
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
  , testGroup
      "Convert to Double"
      [ testCase "Converts word to double" $
        assertEqual
          ""
          987654321
          (doubleAt 2 $
           runSegInitial [0x3c013ade, 0x342868b1, 0x44880000, 0x468000a1])
      , testCase "Converts negative word to double" $
        assertEqual
          ""
          (-42.0)
          (doubleAt 2 $ runSeg [0xffffffd6] [0x3c011001, 0xd4200000, 0x468000a1])
      , testCase "Converts single to double" $
        assertEqual
          ""
          0.0016968456329777837
          (doubleAt 4 $
           runSegInitial [0x3c013ade, 0x342868b1, 0x44880000, 0x46000121])
      , testCase "Converts negative single to double" $
        assertEqual
          ""
          (-3.1415927410125732)
          (doubleAt 2 $ runSeg [0xc0490fdb] [0x3c011001, 0xd4200000, 0x460000a1])
      ]
  , testGroup
      "Convert to Single"
      [ testCase "Converts word to single" $
        assertEqual
          ""
          9.8765434E8
          (floatAt 2 $
           runSegInitial [0x3c013ade, 0x342868b1, 0x44880000, 0x468000a0])
      , testCase "Converts negative word to single" $
        assertEqual
          ""
          (-42.0)
          (floatAt 2 $ runSeg [0xffffffd6] [0x3c011001, 0xd4200000, 0x468000a0])
      , testCase "Converts double to single" $
        assertEqual
          ""
          3.9302653E-25
          (floatAt 2 $
           runSegInitial
             [0x3c013ade, 0x342868b1, 0x44880000, 0x44880800, 0x462000a0])
      , testCase "Converts negative double to single" $
        assertEqual
          ""
          (-pi)
          (floatAt 2 $
           runSeg [0x54442d18, 0xc00921fb] [0x3c011001, 0xd4200000, 0x462000a0])
      ]
  , testGroup
      "Convert to Word"
      [ testCase "Converts single to word" $
        assertEqual
          ""
          0x580B750
          (regAt 9 $
           runSegInitial
             [0x3c014cb0, 0x342816ea, 0x44880000, 0x460000a4, 0x44091000])
      , testCase "Converts single to word" $
        assertEqual
          ""
          2.0
          (floatAt 1 $
           runSeg
             [2]
             [0x3c011001, 0xc4200004, 0x8c280000, 0x44880800, 0x46800860])
      , testCase "Converts double to word" $
        assertEqual
          ""
          3
          (regAt 9 $
           runSeg
             [0x5444261e, 0x400921fb]
             [0x3c011001, 0xd4200000, 0x462000a4, 0x44091000])
      , testCase "Converts negative single to word" $
        assertEqual
          ""
          (tc 3)
          (regAt 8 $
           runSeg [0xc0490fdb] [0x3c011001, 0xd4200000, 0x460000a4, 0x44081000])
      , testCase "Converts negative double to word" $
        assertEqual
          ""
          (tc 3)
          (regAt 8 $
           runSeg
             [0x54442d18, 0xc00921fb]
             [0x3c011001, 0xd4200000, 0x462000a4, 0x44081000])
      ]
  , testGroup
      "FP Add"
      [ testCase "Adds float" $
        assertEqual
          ""
          (2 * pi)
          (floatAt 2 $ runSeg [0x40490fdb] [0x3c011001, 0xc4200000, 0x46000080])
      , testCase "Adds double" $
        assertEqual
          ""
          (2 * pi)
          (doubleAt 2 $
           runSeg [0x54442d18, 0x400921fb] [0x3c011001, 0xd4200000, 0x46200080])
      ]
  , testGroup
      "FP Sub"
      [ testGroup
          "Double"
          [ testCase "42 - pi" $
            assertEqual
              ""
              (42 - pi)
              (doubleAt 4 $
               runSeg
                 [0x54442d18, 0x400921fb, 0, 0x40450000]
                 [0x3c011001, 0xd4200000, 0xd4220008, 0x46201101])
          , testCase "pi - 42" $
            assertEqual
              ""
              (pi - 42)
              (doubleAt 4 $
               runSeg
                 [0x54442d18, 0x400921fb, 0, 0x40450000]
                 [0x3c011001, 0xd4200000, 0xd4220008, 0x46220101])
          , testCase "42 - (-pi)" $
            assertEqual
              ""
              (42 - (-pi))
              (doubleAt 4 $
               runSeg
                 [0x54442d18, 0xc00921fb, 0, 0x40450000]
                 [0x3c011001, 0xd4200000, 0xd4220008, 0x46201101])
          , testCase "(-pi) - 42" $
            assertEqual
              ""
              ((-pi) - 42)
              (doubleAt 4 $
               runSeg
                 [0x54442d18, 0xc00921fb, 0, 0x40450000]
                 [0x3c011001, 0xd4200000, 0xd4220008, 0x46220101])
          , testCase "(-42) - pi" $
            assertEqual
              ""
              ((-42) - pi)
              (doubleAt 4 $
               runSeg
                 [0x54442d18, 0x400921fb, 0, 0xc0450000]
                 [0x3c011001, 0xd4200000, 0xd4220008, 0x46201101])
          , testCase "pi - (-42)" $
            assertEqual
              ""
              (pi - (-42))
              (doubleAt 4 $
               runSeg
                 [0x54442d18, 0x400921fb, 0, 0xc0450000]
                 [0x3c011001, 0xd4200000, 0xd4220008, 0x46220101])
          , testCase "(-42) - (-pi)" $
            assertEqual
              ""
              ((-42) - (-pi))
              (doubleAt 4 $
               runSeg
                 [0x54442d18, 0xc00921fb, 0, 0xc0450000]
                 [0x3c011001, 0xd4200000, 0xd4220008, 0x46201101])
          , testCase "(-pi) - (-42)" $
            assertEqual
              ""
              ((-pi) - (-42))
              (doubleAt 4 $
               runSeg
                 [0x54442d18, 0xc00921fb, 0, 0xc0450000]
                 [0x3c011001, 0xd4200000, 0xd4220008, 0x46220101])
          ]
      ]
  , testGroup
      "FP Mult"
      [ testCase "Multiplies float" $
        assertEqual
          ""
          (7 * pi)
          (floatAt 4 $
           runSeg
             [0x40490fdb]
             [ 0x3c011001
             , 0xc4200000
             , 0x24080007
             , 0x44881000
             , 0x468010a0
             , 0x46001102
             ])
      , testCase "Multiplies double" $
        assertEqual
          ""
          (7 * pi)
          (doubleAt 4 $
           runSeg
             [0x54442d18, 0x400921fb]
             [ 0x3c011001
             , 0xd4200000
             , 0x24080007
             , 0x44881000
             , 0x468010a1
             , 0x46201102
             ])
      ]
  , testGroup
      "FP Div"
      [ testCase "Divides float" $
        assertEqual
          ""
          (7 * pi)
          (floatAt 4 $
           runSeg
             [0x40490fdb]
             [ 0x3c011001
             , 0xc4200000
             , 0x24080007
             , 0x44881000
             , 0x468010a0
             , 0x46001102
             ])
      , testCase "Divides double" $
        assertEqual
          ""
          (pi / 7)
          (doubleAt 4 $
           runSeg
             [0x54442d18, 0x400921fb]
             [ 0x3c011001
             , 0xd4200000
             , 0x24080007
             , 0x44881000
             , 0x468010a1
             , 0x46220103
             ])
      , testCase "Divides double" $
        assertEqual
          ""
          (7 / pi)
          (doubleAt 4 $
           runSeg
             [0x54442d18, 0x400921fb]
             [ 0x3c011001
             , 0xd4200000
             , 0x24080007
             , 0x44881000
             , 0x468010a1
             , 0x46201103
             ])
      , testCase "Converts int and divides" $
        assertEqual
          ""
          0.5
          (floatAt 2 $
           runSeg
             [2, 0x3f800000]
             [ 0x3c011001
             , 0xc4200004
             , 0x8c280000
             , 0x44880800
             , 0x46800860
             , 0x46010083
             ])
      ]
  ]
