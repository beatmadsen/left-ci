module Config.ApplicationConfigTest
  ( tests,
  )
where

import Config.ApplicationConfig
  ( ApplicationConfig (Invalid, Server, Installer),
    parseApplicationConfig,
  )
import Test.HUnit (Test (TestCase, TestLabel, TestList), (@?=))

tests :: Test
tests =
  TestList
    [ TestCase $ do
        let actual = parseApplicationConfig []
        let expected = Invalid "No arguments provided"
        actual @?= expected,
      TestLabel "server command without port flag defaults port to 8585" $ TestCase $ do
        let actual = parseApplicationConfig ["--server"]
        let expected = Server 8585
        actual @?= expected,
      TestCase $ do
        let actual = parseApplicationConfig ["--server", "--port"]
        let expected = Invalid "No port number provided to run the server on. Please add a number after --port"
        actual @?= expected,
      TestLabel "server command with port flag fails if port is not a number" $ TestCase $ do
        let actual = parseApplicationConfig ["--server", "--port", "not-a-number"]
        let expected = Invalid "Invalid port number: not-a-number. Please add a number after --port"
        actual @?= expected,
      TestLabel "server command with port flag sets up server with port" $ TestCase $ do
        let actual = parseApplicationConfig ["--server", "--port", "8080"]
        let expected = Server 8080
        actual @?= expected,
      TestLabel "server command with port flag fails if port is outside of 0-65535" $ TestCase $ do
        let actual = parseApplicationConfig ["--server", "--port", "65536"]
        let expected = Invalid "Invalid port number: 65536. Please add a number after --port"
        actual @?= expected,
      TestLabel "installer command without path fails" $ TestCase $ do
        let actual = parseApplicationConfig ["--installer"]
        let expected = Invalid "No path provided to the project where you want to install Left CI. Please add a path after --installer using --path <path>"
        actual @?= expected,
      TestLabel "installer command with path sets up installer with path" $ TestCase $ do
        let actual = parseApplicationConfig ["--installer", "--path", "/path/to/project"]
        let expected = Installer "/path/to/project"
        actual @?= expected
    ]
