module RunSpec.SystemCallTests where

import           Lib.Computer       (initialComputer)
import           Lib.Computer.Types (Computer)
import           Lib.Operation
import qualified Lib.Registers      as R
import           Lib.Run
import           RunSpec.Helpers
import           Test.Tasty
import           Test.Tasty.HUnit

systemCallTests :: [TestTree]
systemCallTests =
  [ testCase "print 'Ola mundo!'" $
    assertEqual
      ""
      (PutStr "Ola mundo!")
      (getSC
         [0x20616c4f, 0x646e756d, 8559]
         [0x3c011001, 0x34240000, 0x24020004, 0xc])
  , testCase "getint" $ assertEqual "" GetInt (getSC [] [0x24020005, 0xc])
  , testCase "putInt (3+4)" $
    assertEqual
      ""
      (PutInt 7)
      (getSC [] [0x20080003, 0x20090004, 0x01098020, 0x20020001, 0x102020])
  , testCase "die" $ assertEqual "" Die (getSC [] [0x2002000a])
  , testCase "Unaligned putStr is allowed" $
    assertEqual
      ""
      (PutStr "Voc\195\170 digitou: ")
      (getSC
         [0, 0, 0x5600203a, 0xaac3636f, 0x67696420, 0x756f7469, 0x0000203a]
         [0x3c011001, 0x3424000b, 0x24020004, 0xc])
  , testGroup
      "Systemcall Results Correctly Stores"
      [ testCase "Ints" $ assertEqual "" 12 (R.get 2 $ testInput (GotInt 12))
      , testCase "Floats" $
        assertEqual "" pi (R.getF 0 $ testInput (GotFloat pi))
      , testCase "Doubles" $
        assertEqual "" pi (R.getD 0 $ testInput (GotDouble pi))
      ]
  ]

testInput :: SyscallInput -> Computer
testInput inp = storeInput inp $ initialComputer False 0 0 [] [] []
