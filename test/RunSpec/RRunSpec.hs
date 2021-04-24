module RunSpec.RRunSpec
  ( rTests
  ) where

import           Lib.Computer.Types (Computer)
import           RunSpec.Helpers    (regAt, runSegInitial, tc)
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (assertEqual, testCase)

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
  , testGroup
      "Set Less Than"
      [ testCase "slt $t2, 10, 10" $
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
      ]
  , testGroup
      "Set Less Than Unsigned"
      [ testCase "sltu $t2, 42, -200" $
        assertEqual
          ""
          1
          (regAt 10 $ runSegInitial [0x2408002a, 0x2409ff38, 0x0109502b])
      ]
  , testGroup
      "Shift Right Logical"
      [ testCase "srl $t1, 8, 2" $
        assertEqual "" 4 (regAt 9 $ runSegInitial [0x24080010, 0x00084882])
      , testCase "srl $t1, -42, 2" $
        assertEqual
          ""
          1073741813
          (regAt 9 $ runSegInitial [0x2408ffd6, 0x00084882])
      ]
  , testGroup
      "Shift Right Arithmetic"
      [ testCase "sra $t1, 8, 2" $
        assertEqual "" 4 (regAt 9 $ runSegInitial [0x24080010, 0x00084883])
      , testCase "sra $t1, -42, 2" $
        assertEqual
          ""
          (tc 11)
          (regAt 9 $ runSegInitial [0x2408ffd6, 0x00084883])
      ]
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
      "XOr"
      [ testCase "42 ^ 55 == 29" $
        assertEqual
          ""
          29
          (regAt 10 $ runSegInitial [0x2408002a, 0x24090037, 0x01285026])
      , testCase "42 ^ -200 == -238" $
        assertEqual
          ""
          (tc 238)
          (regAt 10 $ runSegInitial [0x2408002a, 0x2409ff38, 0x01285026])
      ]
  , testGroup
      "And"
      [ testCase "42 & 55 == 34" $
        assertEqual
          ""
          34
          (regAt 10 $ runSegInitial [0x2408002a, 0x24090037, 0x01285024])
      , testCase "42 & -200 == 40" $
        assertEqual
          ""
          40
          (regAt 10 $ runSegInitial [0x2408002a, 0x2409ff38, 0x01285024])
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
  , testGroup
      "Subtract unsigned"
      [ testCase "55-42" $
        assertEqual
          ""
          13
          (regAt 10 $ runSegInitial [0x2408002a, 0x24090037, 0x01285023])
      , testCase "42-55" $
        assertEqual
          ""
          (tc 13)
          (regAt 10 $ runSegInitial [0x2408002a, 0x24090037, 0x01095023])
      , testCase "(-55)-42" $
        assertEqual
          ""
          (tc 97)
          (regAt 10 $ runSegInitial [0x2408002a, 0x2409ffc9, 0x01285023])
      , testCase "42-(-55)" $
        assertEqual
          ""
          97
          (regAt 10 $ runSegInitial [0x2408002a, 0x2409ffc9, 0x01095023])
      , testCase "55-(-42)" $
        assertEqual
          ""
          97
          (regAt 10 $ runSegInitial [0x2408ffd6, 0x24090037, 0x01285023])
      , testCase "(-42)-55" $
        assertEqual
          ""
          (tc 97)
          (regAt 10 $ runSegInitial [0x2408ffd6, 0x24090037, 0x01095023])
      , testCase "(-55)-(-42)" $
        assertEqual
          ""
          (tc 13)
          (regAt 10 $ runSegInitial [0x2408ffd6, 0x2409ffc9, 0x01285023])
      , testCase "(-42)-(-55)" $
        assertEqual
          ""
          13
          (regAt 10 $ runSegInitial [0x2408ffd6, 0x2409ffc9, 0x01095023])
      ]
  ]

jrComputer :: Computer
jrComputer =
  runSegInitial
    [0x3c010040, 0x34290014, 0x01200008, 0x2408002a, 0x240b002a, 0x240a0016]
