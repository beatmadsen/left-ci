module Config.ApplicationConfigTest
  ( tests
  ) where

import Test.HUnit ( (@?=), Test(TestCase, TestList) )
import Config.ApplicationConfig
    ( parseApplicationConfig, ApplicationConfig(Invalid) )

tests :: Test
tests = TestList
  [ TestCase $ do
      let actual = parseApplicationConfig []
      let expected = Invalid "No arguments provided"
      actual @?= expected,
    TestCase $ do
      let actual = parseApplicationConfig ["--server"]
      let expected = Invalid "No port number provided to run the server on. Please add --port <port>"
      actual @?= expected
  ]
