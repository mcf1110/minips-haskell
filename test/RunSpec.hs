module RunSpec
  ( runningTests
  ) where

import           Test.Tasty       (TestTree, testGroup)

import           RunSpec.IRunSpec (iTests)
import           RunSpec.JRunSpec (jTests)
import           RunSpec.RRunSpec (rTests)

runningTests :: [TestTree]
runningTests =
  [ testGroup "R Instructions" rTests
  , testGroup "I Instructions" iTests
  , testGroup "J Instructions" jTests
  ]
