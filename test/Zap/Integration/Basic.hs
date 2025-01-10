{-# LANGUAGE OverloadedStrings #-}
module Zap.Integration.Basic
  ( basicTests
  , migratedTests
  ) where

import qualified Data.Text as T
import System.Exit

import Zap.Integration.Types

basicTests :: [TestCase]
basicTests =
  []
  -- [ TestCase
  --     { testName = "simple_block"
  --     , sourceCode = "print \"Before block\"\nblock test:\n  print \"Inside block\""
  --     , expectedOutput = "Before block\nInside block\n"
  --     , expectedExitCode = ExitSuccess
  --     }
  -- , TestCase
  --     { testName = "syntax_error_test"
  --     , sourceCode = "print"  -- Missing string literal
  --     , expectedOutput = ""
  --     , expectedExitCode = ExitFailure 1
  --     }
  -- , TestCase
  --     { testName = "indentation_error_test"
  --     , sourceCode = "block test:\nprint \"Bad indent\""  -- Missing indentation
  --     , expectedOutput = ""
  --     , expectedExitCode = ExitFailure 1
  --     }
  -- , TestCase
  --     { testName = "print_add"
  --     , sourceCode = "print 1 + 1"
  --     , expectedOutput = "2\n"
  --     , expectedExitCode = ExitSuccess
  --     }
  -- , TestCase
  --     { testName = "print_vector_add"
  --     , sourceCode = "let vec1 = Vec3(1.0, 2.0, 3.0)\nlet vec2 = Vec3 4.0, 5.0, 6.0\nprint vec1 + vec2"
  --     , expectedOutput = "(5.000000, 7.000000, 9.000000)\n"
  --     , expectedExitCode = ExitSuccess
  --     }
  -- , TestCase
  --     { testName = "let_expr_block"
  --     , sourceCode = "let\n  v1 = Vec3(1.0, 2.0, 3.0)\n  v2 = Vec3(4.0, 5.0, 6.0)\n\nprint v1 + v2"
  --     , expectedOutput = "(5.000000, 7.000000, 9.000000)\n"
  --     , expectedExitCode = ExitSuccess
  --     }
  -- , TestCase
  --     { testName = "struct_definition"
  --     , sourceCode = "type\n  Point = struct\n    x: f32\n    y: f32\n\nlet origin = Point(1.0, 0.0)\nprint origin.x"
  --     , expectedOutput = "1.000000\n"
  --     , expectedExitCode = ExitSuccess
  --     }
  -- , TestCase
  --     { testName = "while_loop"
  --     , sourceCode = "var n = 0\nwhile n < 3:\n  print n\n  n += 1"
  --     , expectedOutput = "0\n1\n2\n"
  --     , expectedExitCode = ExitSuccess
  --     }
  -- , TestCase
  --     { testName = "function_call_with_params_and_return_val"
  --     , sourceCode = "fn add(x, y: i32): i32 =\n  x + y\n\nprint add(1, 2)"
  --     , expectedOutput = "3\n"
  --     , expectedExitCode = ExitSuccess
  --     }
  -- , TestCase
  --   { testName = "function_with_local_var"
  --   , sourceCode = T.unlines
  --       [ "fn sum_squares(x, y: i32): i32 ="
  --       , "  var sum = x * x"  -- Local variable
  --       , "  sum = sum + y * y"
  --       , "  sum"
  --       , ""
  --       , "print sum_squares(2, 2)"
  --       ]
  --   , expectedOutput = "8\n"
  --   , expectedExitCode = ExitSuccess
  --   }
  -- ]


migratedTests :: [TestCase]
migratedTests =
  [ TestCase
      { testName = "hello_world"
      , sourceCode = "print \"Hello, World!\""
      , expectedOutput = "Hello, World!\n"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_add"
      , sourceCode = "print 1 + 2"
      , expectedOutput = "3\n"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_multiply"
      , sourceCode = "print 2 * 3"
      , expectedOutput = "6\n"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_precedence"
      , sourceCode = "print 2 + 3 * 4"
      , expectedOutput = "14\n"  -- Should evaluate as 2 + (3 * 4)
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_subtract"
      , sourceCode = "print 5 - 3"
      , expectedOutput = "2\n"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_divide"
      , sourceCode = "print 6 / 2"
      , expectedOutput = "3\n"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_mixed_arithmetic"
      , sourceCode = "print 10 - 2 * 3"
      , expectedOutput = "4\n"  -- Should respect operator precedence
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_parentheses_basic"
      , sourceCode = "print (1 + 2) * 3"
      , expectedOutput = "9\n"  -- Without parens would be 7
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_parentheses_nested"
      , sourceCode = "print (2 * (3 + 4))"
      , expectedOutput = "14\n"
      , expectedExitCode = ExitSuccess
      }
  , TestCase
      { testName = "print_call_with_parens"
      , sourceCode = "print(\"Hello, World!\")"
      , expectedOutput = "Hello, World!\n"
      , expectedExitCode = ExitSuccess
      }
    , TestCase
      { testName = "print_comparison_less"
      , sourceCode = "print 2 < 3"
      , expectedOutput = "1\n"  -- true represented as 1
      , expectedExitCode = ExitSuccess
      }
    , TestCase
        { testName = "print_comparison_greater"
        , sourceCode = "print 5 > 3"
        , expectedOutput = "1\n"
        , expectedExitCode = ExitSuccess
        }
    , TestCase
        { testName = "print_comparison_equal"
        , sourceCode = "print 4 == 4"
        , expectedOutput = "1\n"
        , expectedExitCode = ExitSuccess
        }
    , TestCase
        { testName = "print_comparison_precedence"
        , sourceCode = "print 2 + 3 < 10 - 4"
        , expectedOutput = "1\n"  -- (2 + 3) < (10 - 4) -> 5 < 6 -> true
        , expectedExitCode = ExitSuccess
        }
    , TestCase
        { testName = "variable_declaration_and_mutation"
        , sourceCode = T.unlines
            [ "var x = 5"           -- Variable declaration
            , "print x"             -- Initial value
            , "x = x + 3"          -- Direct assignment
            , "print x"             -- After addition
            , "x += 2"             -- Compound assignment
            , "print x"             -- Final value
            ]
        , expectedOutput = T.unlines
            [ "5"                   -- Initial value
            , "8"                   -- After x = x + 3
            , "10"                  -- After x += 2
            ]
        , expectedExitCode = ExitSuccess
        }
    , TestCase
        { testName = "simple_assignment"
        , sourceCode = T.unlines
            [ "var x = 5"
            , "print x"
            ]
        , expectedOutput = "5\n"
        , expectedExitCode = ExitSuccess
        }
    , TestCase
        { testName = "var_declaration_scope"
        , sourceCode = T.unlines
            [ "var x = 5"  -- Variable declaration
            , "print x"    -- Should now be in scope
            ]
        , expectedOutput = "5\n"
        , expectedExitCode = ExitSuccess
        }
    , TestCase
      { testName = "basic_while"
      , sourceCode = T.unlines
          [ "var i = 0"
          , "while i < 3:"
          , "  print i"
          , "  i = i + 1"
          ]
      , expectedOutput = "0\n1\n2\n"
      , expectedExitCode = ExitSuccess
      }
    , TestCase
      { testName = "while_with_break"
      , sourceCode = T.unlines
          [ "var i = 0"
          , "while i < 5:"
          , "  print i"
          , "  i = i + 1"
          , "  if i == 3:"
          , "    break"
          ]
      , expectedOutput = "0\n1\n2\n"
      , expectedExitCode = ExitSuccess
      }
    , TestCase
      { testName = "nested_while_with_boolean"
      , sourceCode = T.unlines
          [ "var i = 0"
          , "var j = 0"
          , "while i < 3:"
          , "  j = 0"
          , "  while j < i:"
          , "    print j"
          , "    j = j + 1"
          , "  i = i + 1"
          ]
      , expectedOutput = "0\n0\n1\n"  -- Should print 0 for i=2, then 0,1 for i=3
      , expectedExitCode = ExitSuccess
      }
    , TestCase
      { testName = "simple_function"
      , sourceCode = T.unlines
          [ "fn add(x, y: i32): i32 ="
          , "  x + y"
          , ""
          , "print add(2, 3)"
          ]
      , expectedOutput = "5\n"
      , expectedExitCode = ExitSuccess
      }
    , TestCase
      { testName = "float32_arithmetic"
      , sourceCode = T.unlines
          [ "print 3.14'f32 + 2.86'f32"  -- Explicit Float32 addition
          ]
      , expectedOutput = "6.000000\n"
      , expectedExitCode = ExitSuccess
      }
    , TestCase
      { testName = "explicit_type_literals"
      , sourceCode = T.unlines
          [ "print 12'i64 + 15'i64"      -- Integer type suffix
          , "print 3.14'f32 + 2.86'f32"  -- Float type suffix
          ]
      , expectedOutput = "27\n6.000000\n"
      , expectedExitCode = ExitSuccess
      }
    , TestCase
      { testName = "int_width_arithmetic"
      , sourceCode = T.unlines
          [ "print 1000'i32 + 2000'i32"  -- 32-bit addition
          , "print 9223372036854775807'i64 - 1'i64"  -- 64-bit subtraction using max i64
          ]
      , expectedOutput = "3000\n9223372036854775806\n"
      , expectedExitCode = ExitSuccess
      }
    , TestCase
        { testName = "basic_struct"
        , sourceCode = T.unlines
            [ "type Point = struct"
            , "  x: i32"
            , "  y: i32"
            , ""
            , "let p = Point(10, 20)"
            , "print p.x"
            ]
        , expectedOutput = "10\n"
        , expectedExitCode = ExitSuccess
        }
    , TestCase
      { testName = "generic_box"
      , sourceCode = T.unlines
          [ "type"
          , "  Box[T] = struct"  -- New syntax for generic type
          , "    value: T"
          , ""
          , "let x = Box[i32](42)"  -- Type parameter instantiation
          , "print x.value"
          ]
      , expectedOutput = "42\n"
      , expectedExitCode = ExitSuccess
      }
    , TestCase
        { testName = "struct_field_names"
        , sourceCode = T.unlines
            [ "type Point = struct"
            , "  x: i32"
            , "  y: i32"
            , ""
            , "let p = Point(10, 20)"
            , "print p.x"
            ]
        , expectedOutput = "10\n"
        , expectedExitCode = ExitSuccess
        }
  ]
