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
      , sourceCode = "print 1 + 1"
      , expectedOutput = "2"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_vector_add"
      , sourceCode = "let vec1 = Vec3(1.0, 2.0, 3.0)\nlet vec2 = Vec3 4.0, 5.0, 6.0\nprint vec1 + vec2"
      , expectedOutput = "(5.000000, 7.000000, 9.000000)"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "let_expr_block"
      , sourceCode = "let\n  v1 = Vec3(1.0, 2.0, 3.0)\n  v2 = Vec3(4.0, 5.0, 6.0)\n\nprint v1 + v2"
      , expectedOutput = "(5.000000, 7.000000, 9.000000)"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "struct_definition"
      , sourceCode = "type\n  Point = struct\n    x: f32\n    y: f32\n\nlet origin = Point(1.0, 0.0)\nprint origin.x"
      , expectedOutput = "1.000000"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "while_loop"
      , sourceCode = "var n = 0\nwhile n < 3:\n  print n\n  n += 1"
      , expectedOutput = ""
      , expectedExitCode = ExitSuccess
      }
  ]
