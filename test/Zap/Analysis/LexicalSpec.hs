{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zap.Analysis.LexicalSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Zap.Analysis.Lexical

spec :: Spec
spec = do
  describe "Lexer" $ do
    it "tokenizes hello world" $ do
      let input = T.pack "print \"Hello, World!\""
      let expected =
            [ Located (TWord "print") 1 1,
              Located (TString "Hello, World!") 7 1,
              Located TEOF 22 1
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes operators" $ do
      let input = T.pack "x + y"
      let expected =
            [ Located (TWord "x") 1 1,
              Located (TOperator "+") 3 1,
              Located (TWord "y") 5 1,
              Located TEOF 6 1
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes complex expressions" $ do
      let input = T.pack "a + b * c"
      let expected =
            [ Located (TWord "a") 1 1,
              Located (TOperator "+") 3 1,
              Located (TWord "b") 5 1,
              Located (TOperator "*") 7 1,
              Located (TWord "c") 9 1,
              Located TEOF 10 1
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes string concatenation" $ do
      let input = T.pack "\"Hello\" + \"World\""
      let expected =
            [ Located (TString "Hello") 1 1,
              Located (TOperator "+") 9 1,
              Located (TString "World") 11 1,
              Located TEOF 18 1
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes block syntax" $ do
      let input = T.pack "block test:"
      let expected =
            [ Located (TWord "block") 1 1,
              Located (TWord "test") 7 1,
              Located TColon 11 1,
              Located TEOF 12 1
            ]
      tokenize input `shouldBe` Right expected

    it "handles nested blocks" $ do
      let input = T.pack "block outer:\n  block inner:"
      let expected =
            [ Located (TWord "block") 1 1,
              Located (TWord "outer") 7 1,
              Located TColon 12 1,
              Located (TWord "block") 3 2,
              Located (TWord "inner") 9 2,
              Located TColon 14 2,
              Located TEOF 15 2
            ]
      tokenize input `shouldBe` Right expected

    it "tracks line numbers" $ do
      let input = T.pack "print\n  \"Hello!\""
      let expected =
            [ Located (TWord "print") 1 1,
              Located (TString "Hello!") 3 2,
              Located TEOF 11 2
            ]
      tokenize input `shouldBe` Right expected

    it "handles unterminated strings" $ do
      let input = T.pack "print \"unterminated"
      tokenize input `shouldBe` Left (UnterminatedString 1 7)

    it "reports error for malformed vector literal" $ do
      let input = T.pack "Vec3(1.0, , 3.0)" -- Missing middle component
      tokenize input `shouldBe` Left (InvalidCharacter ',' 1 10)

    it "tracks position correctly with comments" $ do
      let input =
            T.unlines
              [ "# This is a comment",
                "print \"Hello\"  # End comment"
              ]
      case tokenize input of
        Right tokens ->
          case tokens of
            (pTok : _) -> locLine pTok `shouldBe` 2
            [] -> expectationFailure $ "Parse failed: empty token list"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "handles errors after unterminated string" $ do
      let input = T.pack "print \"unterminated\nprint \"next\""
      tokenize input `shouldBe` Left (UnterminatedString 1 7)

  describe "While loop parsing" $ do
    it "tokenizes while keyword and condition" $ do
      let input = T.pack "while x < 3:"
      let expected =
            [ Located (TWord "while") 1 1,
              Located (TWord "x") 7 1,
              Located (TOperator "<") 9 1,
              Located (TNumber "3") 11 1,
              Located TColon 12 1,
              Located TEOF 13 1
            ]
      tokenize input `shouldBe` Right expected

    it "handles indented while block body" $ do
      let input =
            T.unlines
              [ "while x < 3:",
                "  print x"
              ]
      let expected =
            [ Located (TWord "while") 1 1,
              Located (TWord "x") 7 1,
              Located (TOperator "<") 9 1,
              Located (TNumber "3") 11 1,
              Located TColon 12 1,
              Located (TWord "print") 3 2,
              Located (TWord "x") 9 2,
              Located TEOF 1 3 -- Updated location
            ]
      tokenize input `shouldBe` Right expected

  describe "Operator lexing" $ do
    it "tokenizes simple operators correctly" $ do
      let input = T.pack "a + b"
      let expected =
            [ Located (TWord "a") 1 1,
              Located (TOperator "+") 3 1,
              Located (TWord "b") 5 1,
              Located TEOF 6 1
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes compound += operator" $ do
      let input = T.pack "x += 1"
      let expected =
            [ Located (TWord "x") 1 1,
              Located (TOperator "+=") 3 1,
              Located (TNumber "1") 6 1,
              Located TEOF 7 1
            ]
      tokenize input `shouldBe` Right expected

    it "correctly handles += within expressions" $ do
      let input = T.pack "a += b + c"
      let expected =
            [ Located (TWord "a") 1 1,
              Located (TOperator "+=") 3 1,
              Located (TWord "b") 6 1,
              Located (TOperator "+") 8 1,
              Located (TWord "c") 10 1,
              Located TEOF 11 1
            ]
      tokenize input `shouldBe` Right expected

    it "distinguishes between + and += operators" $ do
      let input = T.pack "a + b += c"
      let expected =
            [ Located (TWord "a") 1 1,
              Located (TOperator "+") 3 1,
              Located (TWord "b") 5 1,
              Located (TOperator "+=") 7 1,
              Located (TWord "c") 10 1,
              Located TEOF 11 1
            ]
      tokenize input `shouldBe` Right expected

    describe "Generic type tokenization" $ do
      it "tokenizes simple generic type declaration" $ do
        let input = T.pack "Box[T]"
        let expected =
              [ Located (TWord "Box") 1 1,
                Located TLeftBracket 4 1,
                Located (TTypeParam "T") 5 1,
                Located TRightBracket 6 1,
                Located TEOF 7 1
              ]
        tokenize input `shouldBe` Right expected

      it "tokenizes type specialization" $ do
        let input = T.pack "Box[i32]"
        let expected =
              [ Located (TWord "Box") 1 1,
                Located TLeftBracket 4 1,
                Located (TWord "i32") 5 1,
                Located TRightBracket 8 1,
                Located TEOF 9 1
              ]
        tokenize input `shouldBe` Right expected

      it "tokenizes multiple type parameters" $ do
        let input = T.pack "Pair[S, T]"
        let expected =
              [ Located (TWord "Pair") 1 1,
                Located TLeftBracket 5 1,
                Located (TTypeParam "S") 6 1,
                Located TComma 7 1,
                Located (TTypeParam "T") 9 1,
                Located TRightBracket 10 1,
                Located TEOF 11 1
              ]
        tokenize input `shouldBe` Right expected

  describe "Pattern matching lexical analysis" $ do
    it "tokenizes basic case expression" $ do
      let input = "case x of:\n  Some(value): 42"
      let expected =
            [ Located (TWord "case") 1 1,
              Located (TWord "x") 6 1,
              Located (TWord "of") 8 1,
              Located TColon 10 1,
              Located (TWord "Some") 3 2,
              Located TLeftParen 7 2,
              Located (TWord "value") 8 2,
              Located TRightParen 13 2,
              Located TColon 14 2,
              Located (TNumber "42") 16 2,
              Located TEOF 18 2
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes None pattern" $ do
      let input = "case x of:\n  None: 0"
      let expected =
            [ Located (TWord "case") 1 1,
              Located (TWord "x") 6 1,
              Located (TWord "of") 8 1,
              Located TColon 10 1,
              Located (TWord "None") 3 2,
              Located TColon 7 2,
              Located (TNumber "0") 9 2,
              Located TEOF 10 2
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes multiple patterns" $ do
      let input = "case x of:\n  Some(0): 1\n  Some(n): 2\n  None: 3"
      let expected =
            [ Located (TWord "case") 1 1,
              Located (TWord "x") 6 1,
              Located (TWord "of") 8 1,
              Located TColon 10 1,
              Located (TWord "Some") 3 2,
              Located TLeftParen 7 2,
              Located (TNumber "0") 8 2,
              Located TRightParen 9 2,
              Located TColon 10 2,
              Located (TNumber "1") 12 2,
              Located (TWord "Some") 3 3,
              Located TLeftParen 7 3,
              Located (TWord "n") 8 3,
              Located TRightParen 9 3,
              Located TColon 10 3,
              Located (TNumber "2") 12 3,
              Located (TWord "None") 3 4,
              Located TColon 7 4,
              Located (TNumber "3") 9 4,
              Located TEOF 10 4
            ]
      tokenize input `shouldBe` Right expected
