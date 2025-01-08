module Config.ApplicationConfigTest
  ( tests
  ) where

import Test.HUnit
import Config.ApplicationConfig

tests :: Test
tests = TestList
  [ TestCase $ do
      let actual = parseApplicationConfig []
      let expected = Invalid
      actual @?= expected
  ]
