module RunSpec.IRunSpec
  ( iTests
  ) where

import           Lib.Computer     (Computer)
import           RunSpec.Helpers  (doubleAt, floatAt, regAt, runSeg,
                                   runSegInitial)
import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

iTests :: [TestTree]
iTests =
  [ testCase "Cannot overwrite $zero" $
    assertEqual "" 0 (regAt 0 $ runSegInitial [0x24000003])
  , testGroup
      "Add Immediate"
      [ testCase "addi $t0, $zero, 3 - loads 3 to $t0" $
        assertEqual "" 3 (regAt 8 $ runSegInitial [0x20080003])
      , testCase "addi $t1, $zero, 4 - loads 4 to $t1" $
        assertEqual "" 4 (regAt 9 $ runSegInitial [0x20090004])
      ]
  , testGroup
      "Add Immediate Upper"
      [ testCase "addiu $sp, $sp, -32" $
        assertEqual "" 0x7fffefdc (regAt 29 $ runSegInitial [0x27bdffe0])
      ]
  , testCase "add 3 4" $
    assertEqual
      ""
      7
      (regAt 16 $ runSegInitial [0x20080003, 0x20090004, 0x01098020])
  , testCase "lui $a0, 0x1001" $
    assertEqual "" 0x10010000 (regAt 1 $ runSegInitial [0x3c011001])
  , testCase "ori $a0, $at, 0" $
    assertEqual "" 0x10010000 (regAt 4 $ runSegInitial [0x3c011001, 0x34240000])
  , testCase "ori $t1, $t0, 16" $
    assertEqual "" 0x10 (regAt 9 $ runSegInitial [0x24080014, 0x31090010])
  , testGroup
      "Branch Delay"
      [ testCase "Branch Delay" $
        assertEqual
          "Executes instruction in BDS"
          1
          (regAt 9 $
           runSegInitial [0x10000002, 0x24090001, 0x240a0002, 0x240b0003])
      , testCase "Does not execute BDS+1" $
        assertEqual
          ""
          0
          (regAt 10 $
           runSegInitial [0x10000002, 0x24090001, 0x240a0002, 0x240b0003])
      , testCase "Branches accordingly" $
        assertEqual
          ""
          3
          (regAt 11 $
           runSegInitial [0x10000002, 0x24090001, 0x240a0002, 0x240b0003])
      ]
  , testGroup
      "Branch on Equal"
      [ testCase "Changes PC correctly" $
        assertEqual
          ""
          (0x00400000 + (2 * 4))
          (regAt 32 $ runSegInitial [0x10000001])
      , testCase "Executes correct instructions" $
        assertEqual
          ""
          0x0
          (regAt 4 $ runSegInitial [0x10000002, 0x0, 0x3c011001, 0x34240000])
      ]
  , testGroup
      "Branch Not Equal - bne 1, $zero, 2"
      [ testCase "BDS" $
        assertEqual
          ""
          15
          (regAt 9 $
           runSegInitial
             [0x24090001, 0x15200002, 0x2409000f, 0x240a0002, 0x240b0003])
      , testCase "BDS+1" $
        assertEqual
          ""
          0
          (regAt 10 $
           runSegInitial
             [0x24090001, 0x15200002, 0x2409000f, 0x240a0002, 0x240b0003])
      , testCase "Branches accordingly" $
        assertEqual
          ""
          3
          (regAt 11 $
           runSegInitial
             [0x24090001, 0x15200002, 0x2409000f, 0x240a0002, 0x240b0003])
      ]
  , testGroup
      "Load Word"
      [ testCase "lw $t0, ans" $
        assertEqual "" 0x2a (regAt 8 $ runSeg [0x2a] [0x3c011001, 0x8c280000])
      , testCase "lw $t1, 4($t0)" $
        assertEqual
          ""
          0x30303234
          (regAt 9 $
           runSeg [0x70736552, 0x30303234] [0x3c011001, 0x34280000, 0x8d090004])
      ]
  , testGroup
      "Load Byte"
      [ testCase "First byte" $
        assertEqual "" 0xffffffdd (regAt 8 loadByteComputer)
      , testCase "Second byte" $
        assertEqual "" 0xffffffcc (regAt 9 loadByteComputer)
      , testCase "Third byte" $
        assertEqual "" 0xffffffbb (regAt 10 loadByteComputer)
      , testCase "Fourth byte" $
        assertEqual "" 0xffffffaa (regAt 11 loadByteComputer)
      ]
  , testCase "sw $t0, ans" $
    assertEqual
      ""
      0x2a
      (regAt 8 $ runSegInitial [0x2408002a, 0x3c011001, 0xac280000])
  , testGroup
      "slti"
      [ testCase "Sets reg to True" $
        assertEqual "" 1 (regAt 9 $ runSegInitial [0x2408002a, 0x29090032])
      , testCase "Sets reg to False" $
        assertEqual "" 0 (regAt 9 $ runSegInitial [0x2408002a, 0x2909001e])
      , testCase "Deals with negative numbers" $
        assertEqual "" 0 (regAt 9 $ runSegInitial [0x2408fff4, 0x2909ffd6])
      , testCase "Deals with negative numbers pt2" $
        assertEqual "" 1 (regAt 9 $ runSegInitial [0x2408ffd6, 0x2909fff4])
      ]
  , testGroup
      "Load Word to Floating Point"
      [ testCase "Sets $f0 to pi" $
        assertEqual
          ""
          pi
          (floatAt 0 $ runSeg [0x40490fdb] [0x3c011001, 0xc4200000])
      ]
  , testGroup
      "Load Double Word to Floating Point"
      [ testCase "Sets $f0 to pi as double" $
        assertEqual
          ""
          pi
          (doubleAt 0 $ runSeg [0x54442d18, 0x400921fb] [0x3c011001, 0xd4200000])
      , testCase "Sets $f0 to 3.31e12 as float" $
        assertEqual
          ""
          3.37028055E12
          (floatAt 0 $ runSeg [0x54442d18, 0x400921fb] [0x3c011001, 0xd4200000])
      , testCase "Sets $f1 to 2.14 as float" $
        assertEqual
          ""
          2.142699
          (floatAt 1 $ runSeg [0x54442d18, 0x400921fb] [0x3c011001, 0xd4200000])
      ]
  , testGroup
      "Store Word from Floating Point"
      [ testCase "Can store 2*pi" $
        assertEqual
          ""
          0x40c90fdb
          (regAt 15 $
           runSeg
             [0x40490fdb]
             [0x3c011001, 0xc4200000, 0x46000000, 0xe4200000, 0x8c2f0000])
      ]
  ]

loadByteComputer :: Computer
loadByteComputer =
  runSeg
    [0xaabbccdd]
    [ 0x3c011001
    , 0x342f0000
    , 0x81e80000
    , 0x81e90001
    , 0x81ea0002
    , 0x81eb0003
    , 0x81ec0004
    ]
