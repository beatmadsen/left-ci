module Server.ServiceTest(tests) where

import Test.HUnit
import Data.Time (UTCTime)
import Data.Text (Text, pack)
import Server.Service (parseAfter)

validAfter :: Text
validAfter = pack "2025-02-22T10%3A44%3A40.160Z"

tests :: Test
tests = TestList [
    TestLabel "parseAfter returns the correct time" testParseValid,
    TestLabel "parseAfter returns an error for an invalid time" testParseInvalid
    ]

testParseValid :: Test
testParseValid = TestCase $ do
    let result = parseAfter validAfter
    assertEqual "parseAfter should return the correct time" 
        (Right $ read "2025-02-22 10:44:40.160Z") result

testParseInvalid :: Test
testParseInvalid = TestCase $ do
    let result = parseAfter $ pack "xyz"
    assertEqual "parseAfter should return an error for an invalid time" (Left "xyz") result