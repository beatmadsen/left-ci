module Config.ApplicationConfigTest
  ( tests,
  )
where

import Config.ApplicationConfig
  ( ApplicationConfig (Installer, Invalid, Server),
    validate,
  )
import Test.HUnit (Test (TestCase, TestLabel, TestList), (@?=))

tests :: Test
tests =
  TestList
    [ TestLabel "validate returns Invalid if the application config is invalid" $ TestCase $ do
        actual <- validate $ Invalid "x"
        let expected = Invalid "x"
        actual @?= expected
    ]