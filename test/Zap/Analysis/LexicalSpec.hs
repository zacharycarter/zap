{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Zap.Analysis.LexicalSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Zap.Analysis.Lexical

spec :: Spec
spec = do
  describe "Lexer" $ do
    it "tokenizes hello world" $ do
      let input = T.pack "print \"Hello, World!\""
      let expected = [
            Located (TWord "print") 1 1,
            Located (TString "Hello, World!") 7 1,
            Located TEOF 22 1
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes operators" $ do
      let input = T.pack "x + y"
      let expected = [
            Located (TWord "x") 1 1,
            Located (TOperator "+") 3 1,
            Located (TWord "y") 5 1,
            Located TEOF 6 1
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes complex expressions" $ do
      let input = T.pack "a + b * c"
      let expected = [
            Located (TWord "a") 1 1,
            Located (TOperator "+") 3 1,
            Located (TWord "b") 5 1,
            Located (TOperator "*") 7 1,
            Located (TWord "c") 9 1,
            Located TEOF 10 1
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes string concatenation" $ do
      let input = T.pack "\"Hello\" + \"World\""
      let expected = [
            Located (TString "Hello") 1 1,
            Located (TOperator "+") 9 1,
            Located (TString "World") 11 1,
            Located TEOF 18 1
            ]
      tokenize input `shouldBe` Right expected

    it "tokenizes block syntax" $ do
      let input = T.pack "block test:"
      let expected = [
            Located (TWord "block") 1 1,
            Located (TWord "test") 7 1,
            Located TColon 11 1,
            Located TEOF 12 1
            ]
      tokenize input `shouldBe` Right expected

    it "handles nested blocks" $ do
      let input = T.pack "block outer:\n  block inner:"
      let expected = [
            Located (TWord "block") 1 1,
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
      let expected = [
            Located (TWord "print") 1 1,
            Located (TString "Hello!") 3 2,
            Located TEOF 11 2
            ]
      tokenize input `shouldBe` Right expected

    it "handles unterminated strings" $ do
      let input = T.pack "print \"unterminated"
      tokenize input `shouldBe` Left (UnterminatedString 1 7)
