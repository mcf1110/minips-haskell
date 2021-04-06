module RunSpec.RRunSpec
  ( rTests
  ) where

import           Lib.Computer     (Computer)
import           RunSpec.Helpers  (regAt, runSegInitial, tc)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

rTests :: [TestTree]
rTests =
  [ testGroup
      "Jump register"
      [ testCase "Works with jal" $
        assertEqual
          ""
          11
          (regAt 9 $
           runSegInitial
             [0x0c100005, 0, 0x2409000b, 0x08100007, 0, 0x03e00008, 0])
      , testCase "Changes PC" $
        assertEqual "" (0x00400000 + 6 * 4) (regAt 32 jrComputer)
      , testCase "Jumps correctly" $ assertEqual "" 22 (regAt 10 jrComputer)
      , testCase "BDL" $ assertEqual "" 42 (regAt 8 jrComputer)
      , testCase "Not BDL+1" $ assertEqual "" 0 (regAt 11 jrComputer)
      ]
  , testCase "slt $t2, 10, 10" $
    assertEqual
      ""
      0
      (regAt 10 $ runSegInitial [0x2408000a, 0x2409000a, 0x0109502a])
  , testCase "slt $t2, 2, 10" $
    assertEqual
      ""
      1
      (regAt 10 $ runSegInitial [0x24080002, 0x2409000a, 0x0109502a])
  , testCase "slt $t3, 0xffffffff, 0" $
    assertEqual
      ""
      1
      (regAt 11 $ runSegInitial [0x24080000, 0x2409ffff, 0x0128582a])
  , testCase "srl $t1, $t0, 2" $
    assertEqual "" 4 (regAt 9 $ runSegInitial [0x24080010, 0x00084882])
  , testCase "sll $t1, $t0, 2" $
    assertEqual "" 0x40 (regAt 9 $ runSegInitial [0x24080010, 0x00084880])
  , testGroup
      "Or"
      [ testCase "42 | 17 == 59" $
        assertEqual
          ""
          59
          (regAt 11 $ runSegInitial [0x2409002a, 0x240a0011, 0x012a5825])
      , testCase "42 | -200 == -192" $
        assertEqual
          ""
          (tc 198)
          (regAt 11 $ runSegInitial [0x2409002a, 0x240aff38, 0x012a5825])
      ]
  , testGroup
      "Mult"
      [ testGroup
          "42*42 == 1764"
          [ testCase "lo" $
            assertEqual
              ""
              1764
              (regAt 34 $ runSegInitial [0x2409002a, 0x240a002a, 0x012a0018])
          , testCase "hi" $
            assertEqual
              ""
              0
              (regAt 33 $ runSegInitial [0x2409002a, 0x240a002a, 0x012a0018])
          ]
      , testGroup
          "42*-42 == -1764"
          [ testCase "lo" $
            assertEqual
              ""
              (tc 1764)
              (regAt 34 $ runSegInitial [0x2409002a, 0x240affd6, 0x012a0018])
          , testCase "hi" $
            assertEqual
              ""
              (tc 1)
              (regAt 33 $ runSegInitial [0x2409002a, 0x240affd6, 0x012a0018])
          ]
      , testGroup
          "-42*-42 == -1764"
          [ testCase "lo" $
            assertEqual
              ""
              1764
              (regAt 34 $ runSegInitial [0x2409ffd6, 0x240affd6, 0x012a0018])
          , testCase "hi" $
            assertEqual
              ""
              0
              (regAt 33 $ runSegInitial [0x2409ffd6, 0x240affd6, 0x012a0018])
          ]
      ]
  , testGroup
      "Div"
      [ testGroup
          "42/11"
          [ testCase "lo" $
            assertEqual
              ""
              3
              (regAt 34 $ runSegInitial [0x2409002a, 0x240a000b, 0x012a001a])
          , testCase "hi" $
            assertEqual
              ""
              9
              (regAt 33 $ runSegInitial [0x2409002a, 0x240a000b, 0x012a001a])
          ]
      , testGroup
          "-42/11"
          [ testCase "lo" $
            assertEqual
              ""
              (tc 3)
              (regAt 34 $ runSegInitial [0x2409ffd6, 0x240a000b, 0x012a001a])
          , testCase "hi" $
            assertEqual
              ""
              (tc 9)
              (regAt 33 $ runSegInitial [0x2409ffd6, 0x240a000b, 0x012a001a])
          ]
      , testGroup
          "-42/-11"
          [ testCase "lo" $
            assertEqual
              ""
              3
              (regAt 34 $ runSegInitial [0x2409ffd6, 0x240afff5, 0x012a001a])
          , testCase "hi" $
            assertEqual
              ""
              (tc 9)
              (regAt 33 $ runSegInitial [0x2409ffd6, 0x240afff5, 0x012a001a])
          ]
      ]
  , testGroup
      "Move from low"
      [ testCase "mflo $t3" $
        assertEqual
          ""
          242
          (regAt 11 $
           runSegInitial [0x2409000b, 0x240a0016, 0x012a0018, 0x00005812])
      ]
  , testGroup
      "Move from high"
      [ testCase "mfhi $t3" $
        assertEqual
          ""
          (tc 1)
          (regAt 11 $
           runSegInitial [0x2409000b, 0x240affea, 0x012a0018, 0x00005810])
      ]
  ]

jrComputer :: Computer
jrComputer =
  runSegInitial
    [0x3c010040, 0x34290014, 0x01200008, 0x2408002a, 0x240b002a, 0x240a0016]
