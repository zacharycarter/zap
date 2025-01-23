{-# LANGUAGE OverloadedStrings #-}

module Zap.Integration.Types
  ( TestCase (..),
    TestResult (..),
    compareOutput,
  )
where

import qualified Data.Text as T
import System.Exit

data TestCase = TestCase
  { testName :: String,
    sourceCode :: T.Text,
    expectedOutput :: T.Text,
    expectedExitCode :: ExitCode
  }

data TestResult
  = TestSuccess
  | TestFailure String
  | TestError String
  | CompilationFailure String
  | RuntimeFailure ExitCode String
  deriving (Show, Eq)

compareOutput :: T.Text -> T.Text -> TestResult
compareOutput expected actual
  | expected == actual = TestSuccess
  | otherwise =
      TestFailure $
        "Output mismatch:\nExpected: "
          ++ T.unpack expected
          ++ "\nActual: "
          ++ T.unpack actual
