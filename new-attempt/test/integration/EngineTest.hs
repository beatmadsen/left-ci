module Integration.EngineTest (tests) where

import Test.HUnit
import Engine

testServerMode :: Test
testServerMode = TestCase $ do
  let args = ["server"]
  mode <- parseMode args
  assertEqual "Server mode should be recognized" 
    ServerMode
    mode

tests :: Test
tests = TestList [
  TestLabel "Server Mode Test" testServerMode
  ] 