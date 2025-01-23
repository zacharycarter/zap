{-# LANGUAGE OverloadedStrings #-}

module Zap.Integration.IntegrationSpec (spec) where

import Control.Monad (forM_)
import System.Exit
import Test.Hspec
import Zap.Integration.Basic
import Zap.Integration.Runner
import Zap.Integration.Types

spec :: Spec
spec = do
  describe "End-to-end compilation and execution" $ do
    -- Run each basic test as a separate test case
    forM_ basicTests $ \test ->
      it ("compiles and runs " ++ testName test) $ do
        result <- runTest' test
        result `shouldBe` TestSuccess

    forM_ migratedTests $ \test ->
      it ("compiles and runs " ++ testName test) $ do
        result <- runTest' test
        result `shouldBe` TestSuccess

    describe "Error handling" $ do
      it "handles syntax errors gracefully" $ do
        let badTest =
              TestCase
                { testName = "syntax_error",
                  sourceCode = "print", -- Missing string literal
                  expectedOutput = "",
                  expectedExitCode = ExitFailure 1
                }
        result <- runTest' badTest
        result `shouldBe` TestSuccess
