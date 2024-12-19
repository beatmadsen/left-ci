import Test.HUnit
import qualified Integration.EngineTest

main :: IO Counts
main = runTestTT $ TestList [
    TestLabel "Engine Integration Tests" Integration.EngineTest.tests
  ]
