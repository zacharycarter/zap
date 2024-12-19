{-# LANGUAGE OverloadedStrings #-}
module Zap.Integration.IntegrationSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import System.Exit
import Control.Monad (forM_)

import Zap.Integration
import Zap.Integration.Basic
import Zap.Parser.Core (ParseError(..))

spec :: Spec
spec = do
  describe "End-to-end compilation and execution" $ do
    -- Run each basic test as a separate test case
    forM_ basicTests $ \test ->
      it ("compiles and runs " ++ testName test) $ do
        result <- runTest test
        result `shouldBe` TestSuccess

    describe "Error handling" $ do
      it "handles syntax errors gracefully" $ do
        let badTest = TestCase
              { testName = "syntax_error"
              , sourceCode = "print" -- Missing string literal
              , expectedOutput = ""
              , expectedExitCode = ExitFailure 1
              }
        result <- runTest badTest
        result `shouldBe` TestSuccess
