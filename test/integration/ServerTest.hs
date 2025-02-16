{-# LANGUAGE OverloadedStrings #-}

module ServerTest
  ( tests,
  )
where

import Test.HUnit
import Control.Exception (bracket)
import System.Directory (removeDirectoryRecursive, doesFileExist)
import RandomHelper (getUniqueDirName)


tests :: Test
tests =
  TestList
    [ TestLabel "Given x, when y, then z" testX]

testX :: Test
testX = TestCase $ do
  return ()
