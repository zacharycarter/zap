{-# LANGUAGE OverloadedStrings #-}
module Zap.Integration.Basic (basicTests) where

import Zap.Integration
import qualified Data.Text as T
import System.Exit

basicTests :: [TestCase]
basicTests =
  [ TestCase
      { testName = "hello_world"
      , sourceCode = "print \"Hello, World!\""
      , expectedOutput = "Hello, World!\n"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "simple_block"
      , sourceCode = "print \"Before block\"\nblock test:\n  print \"Inside block\""
      , expectedOutput = "Before block\nInside block\n"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "syntax_error_test"
      , sourceCode = "print"  -- Missing string literal
      , expectedOutput = ""
      , expectedExitCode = ExitFailure 1
      }
  , TestCase
      { testName = "indentation_error_test"
      , sourceCode = "block test:\nprint \"Bad indent\""  -- Missing indentation
      , expectedOutput = ""
      , expectedExitCode = ExitFailure 1
      }
  , TestCase
      { testName = "print_add"
      , sourceCode = "print 1 + 1"  -- Missing indentation
      , expectedOutput = "2"
      , expectedExitCode = ExitSuccess
      }
  ]
