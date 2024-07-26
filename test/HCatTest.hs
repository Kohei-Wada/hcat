module Main where

import HCat
import Test.HUnit
import qualified System.Exit as Exit

testHandleArgs :: Test
testHandleArgs = TestList
  [ "test1" ~: handleArgs ["file.txt"] @?= Right "file.txt"
  , "test2" ~: handleArgs [] @?= Left "No file specified"
  , "test3" ~: handleArgs ["file1.txt", "file2.txt"] @?= Left "Too many arguments"
  ]

main :: IO ()
main = do
  results <- runTestTT $ TestList [testHandleArgs]
  if failures results == 0
    then Exit.exitSuccess
    else Exit.exitFailure
