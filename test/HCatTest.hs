module Main where

import HCat
import qualified System.Exit as Exit
import Test.HUnit

testHandleArgs :: Test
testHandleArgs =
  TestList
    [ "test1" ~: handleArgs ["file.txt"] @?= Right ["file.txt"],
      "test2" ~: handleArgs [] @?= Left "No files specified",
      "test3" ~: handleArgs ["file1.txt", "file2.txt"] @?= Right ["file1.txt", "file2.txt"]
    ]

testGroupsOf :: Test
testGroupsOf =
  TestList
    [ "test1" ~: groupsOf 2 [1, 2, 3, 4, 5] @?= [[1, 2], [3, 4], [5]],
      "test2" ~: groupsOf 3 [1, 2, 3, 4, 5] @?= [[1, 2, 3], [4, 5]],
      "test3" ~: groupsOf 1 [1, 2, 3, 4, 5] @?= [[1], [2], [3], [4], [5]]
    ]

main :: IO ()
main = do
  results <-
    runTestTT $
      TestList
        [ testHandleArgs,
          testGroupsOf
        ]
  if failures results == 0
    then Exit.exitSuccess
    else Exit.exitFailure
