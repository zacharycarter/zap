{-# LANGUAGE OverloadedStrings #-}
module Zap.Integration.Runner (runTests) where

import Zap.Integration
import Control.Monad (forM)
import System.IO
import Text.Printf

runTests :: [TestCase] -> IO Bool
runTests tests = do
  printf "Running %d integration tests\n" (length tests)
  results <- forM tests $ \test -> do
    printf "Test: %s..." (testName test)
    hFlush stdout
    result <- runTest test
    case result of
      TestSuccess -> do
        putStrLn " PASS"
        return True
      TestFailure msg -> do
        putStrLn " FAIL"
        putStrLn msg
        return False
      TestError msg -> do
        putStrLn " ERROR"
        putStrLn msg
        return False

  let passed = length $ filter id results
  let total = length tests
  printf "\nPassed %d of %d tests\n" passed total
  return $ passed == total
