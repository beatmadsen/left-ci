module Config.ApplicationConfigTest
  ( tests,
  )
where

import Config.ApplicationConfig
  ( ApplicationConfig (Installer, Invalid, Server),
    validate,
  )
import Test.HUnit (Test (TestCase, TestLabel, TestList), (@?=))
import Network.Socket
    ( Socket,
      SocketType(Stream),
      socket,
      bind,
      defaultProtocol,
      tupleToHostAddress,
      SockAddr(SockAddrInet),
      Family(AF_INET),
      close )

occupyPort :: Int -> IO Socket
occupyPort port = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
    return sock



tests :: Test
tests =
  TestList
    [ TestLabel "validate returns Invalid if the application config is invalid" $ TestCase $ do
        actual <- validate $ Invalid "x"
        let expected = Invalid "x"
        actual @?= expected,
      TestLabel "validate returns Invalid if server port is not available" $ TestCase $ do
        sock <- occupyPort 8080
        actual <- validate $ Server 8080
        close sock
        let expected = Invalid "Server port is not available"
        actual @?= expected,
      TestLabel "validate returns Server if server port is available" $ TestCase $ do
        actual <- validate $ Server 8081
        let expected = Server 8081
        actual @?= expected
    ]
