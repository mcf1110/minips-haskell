module RunSpec.JRunSpec
  ( jTests
  ) where

import           RunSpec.Helpers  (regAt, runSegInitial)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

jTests :: [TestTree]
jTests =
  [ testGroup
      "Jump"
      [ testCase "j 0x40000008" $
        assertEqual
          ""
          0
          (regAt 4 $ runSegInitial [0x08100003, 0, 0x3c011001, 0x34240000])
      , testCase "j 0x40000008" $
        assertEqual
          ""
          0x400010
          (regAt 32 $ runSegInitial [0x08100003, 0, 0x3c011001, 0x34240000])
      , testCase "BDL" $
        assertEqual
          ""
          11
          (regAt 9 $
           runSegInitial [0x08100003, 0x2409000b, 0x240a0016, 0x240b0021])
      , testCase "BDL+1" $
        assertEqual
          ""
          0
          (regAt 10 $
           runSegInitial [0x08100003, 0x2409000b, 0x240a0016, 0x240b0021])
      , testCase "Jumps to instruction" $
        assertEqual
          ""
          33
          (regAt 11 $
           runSegInitial [0x08100003, 0x2409000b, 0x240a0016, 0x240b0021])
      ]
  , testGroup
      "Jump and Link - jal 0x00400008"
      [ testCase "Sets PC" $
        assertEqual "" (0x00400000 + 8) (regAt 32 $ runSegInitial [0x0c100002])
      , testCase "Sets $ra" $
        assertEqual "" (0x00400000 + 8) (regAt 31 $ runSegInitial [0x0c100002])
      , testCase "BDS runs" $
        assertEqual
          ""
          11
          (regAt 9 $
           runSegInitial [0x0c100003, 0x2409000b, 0x2409002a, 0x240b0021])
      , testCase "BDS does jump" $
        assertEqual
          ""
          33
          (regAt 11 $
           runSegInitial [0x0c100003, 0x2409000b, 0x2409002a, 0x240b0021])
      , testCase "BDS sets $ra" $
        assertEqual
          ""
          (0x00400000 + 8)
          (regAt 31 $
           runSegInitial [0x0c100003, 0x2409000b, 0x2409002a, 0x240b0021])
      , testCase "BDS sets PC" $
        assertEqual
          ""
          (0x00400000 + 0x10)
          (regAt 32 $
           runSegInitial [0x0c100003, 0x2409000b, 0x2409002a, 0x240b0021])
      ]
  , testGroup
      "Jalr"
      [ testCase "BDS" $ assertEqual "" 42 (regAt 9 jalrComputer)
      , testCase "$t2" $ assertEqual "" 0 (regAt 10 jalrComputer)
      , testCase "$t3" $ assertEqual "" 33 (regAt 11 jalrComputer)
      , testCase "$t4" $ assertEqual "" 44 (regAt 12 jalrComputer)
      ]
  ]

jalrComputer =
  runSegInitial
    [ 0x3c010040 --la      $t0, lbl
    , 0x3428001c
    , 0x01008009 -- jalr    $s0, $t0
    , 0x24090020 -- li $t1, 32
    , 0x240c002c -- li $t4, 44
    , 0x3c010040 -- la $s0, fim
    , 0x34300024
    , 0x21290005 -- addi $t1, $t1, 21
    , 0x02000008 -- jr       $s0
    , 0x240b0021 -- li $t3, 33
    ]
