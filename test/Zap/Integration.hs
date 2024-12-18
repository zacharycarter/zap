{-# LANGUAGE OverloadedStrings #-}
module Zap.Integration
  ( TestCase(..)
  , TestResult(..)
  , runTest
  , compareOutput
  ) where

import qualified Data.Text as T
import System.Exit
import Control.Monad.IO.Class

import Zap.Compiler
import Zap.IR.Core
import Zap.Codegen.C
import Zap.Parser.Core (ParseError(..))

data TestCase = TestCase
  { testName :: String
  , sourceCode :: T.Text
  , expectedOutput :: T.Text
  , expectedExitCode :: ExitCode
  }

data TestResult
  = TestSuccess
  | TestFailure String
  | TestError String
  deriving (Show, Eq)

runTest :: TestCase -> IO TestResult
runTest test = do
  let compileResult = compile defaultCompileOptions (sourceCode test)
  pure $ case (compileResult, expectedExitCode test) of
    -- For expected successes (ExitSuccess)
    (Right result, ExitSuccess) ->
      case generatedCode result of
        Just _ -> TestSuccess
        Nothing -> TestFailure "No code generated when success was expected"

    -- For expected failures (ExitFailure)
    (Left _, ExitFailure _) -> TestSuccess

    -- For unexpected successes (should have failed)
    (Right _, ExitFailure _) ->
      TestFailure "Compilation succeeded when failure was expected"

    -- For unexpected failures (should have succeeded)
    (Left err, ExitSuccess) ->
      TestFailure $ "Compilation failed unexpectedly: " ++ show err

-- | Helper to determine if a compilation result represents a syntax error
isSyntaxError :: CompileError -> Bool
isSyntaxError (ParserError (UnexpectedToken _ _)) = True
isSyntaxError (ParserError (EndOfInput _)) = True
isSyntaxError _ = False

-- | Helper to determine if a compilation result represents an indentation error
isIndentationError :: CompileError -> Bool
isIndentationError (ParserError (IndentationError _ _ _)) = True
isIndentationError _ = False

compareOutput :: T.Text -> T.Text -> TestResult
compareOutput expected actual
  | expected == actual = TestSuccess
  | otherwise = TestFailure $ "Output mismatch:\nExpected: " ++ T.unpack expected ++
                             "\nActual: " ++ T.unpack actual
