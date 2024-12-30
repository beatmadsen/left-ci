module ServerTest where

import Test.HUnit
import Server.App (app, runWithArgsAndServer)
import FakeWarp (fakeWarp)

serverTests :: Test
serverTests = TestList
    [ TestLabel "Server Integration" $ TestCase $ do
        runWithArgsAndServer [] fakeWarp
        -- Add assertions based on captured output
    ]
