{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zap.Parser.ParserSpec (spec) where

import Test.Hspec
import Control.Monad.State
import Data.Either (isLeft)
import qualified Data.Text as T


import Zap.Analysis.Lexical
import Zap.Parser.Types
import Zap.Parser.Core
import Zap.Parser.Expr
import Zap.Parser.Program
import Zap.AST
import Debug.Trace

parseTextExpr :: T.Text -> Either ParseError Expr
parseTextExpr input = do
  tokens <- mapLexError $ tokenize input
  runParser (parseExpr defaultExprParser) tokens
  where
    mapLexError (Left (UnterminatedString line col)) =
      Left $ EndOfInput $ "Unterminated string at line " ++ show line ++ ", column " ++ show col
    mapLexError (Left (InvalidCharacter c line col)) =
      Left $ EndOfInput $ "Invalid character '" ++ [c] ++ "' at line " ++ show line ++ ", column " ++ show col
    mapLexError (Right a) = Right a

spec :: Spec
spec = do
  describe "Indentation checking" $ do
    it "enforces Equal relation" $ do
      let tokens = [Located (TWord "x") 2 1]
      let parser = checkIndent Equal
      runParser parser tokens `shouldBe`
        Left (IndentationError 0 2 Equal)

    it "enforces Greater relation" $ do
      let tokens = [Located (TWord "x") 1 1]
      let parser = do
            modify (\s -> s { stateIndent = 2 })
            checkIndent Greater
      runParser parser tokens `shouldBe`
        Left (IndentationError 2 1 Greater)

    it "allows any indentation with Any" $ do
      let tokens = [Located (TWord "x") 10 1]
      runParser (checkIndent Any) tokens `shouldBe` Right ()

  describe "Token matching" $ do
    it "matches word tokens" $ do
      let tokens = [Located (TWord "print") 1 1]
      let parser = matchToken (\case TWord "print" -> True; _ -> False) "print"
      runParser parser tokens `shouldBe` Right (head tokens)

  describe "Print statement parsing" $ do
    it "parses hello world" $ do
      let input = "print \"Hello, World!\""
      case parseProgram (T.pack input) of
        Right (print, expr) -> do
          locToken print `shouldBe` TWord "print"
          locToken expr `shouldBe` TString "Hello, World!"
        Left err -> fail $ "Parse failed: " ++ show err

  describe "Block parsing" $ do
    it "parses simple block" $ do
      let input = "block test:\n  \"Hello\""
      case parseTextExpr (T.pack input) of
        Right (Block scope) -> do
          blockLabel scope `shouldBe` "test"
          blockExprs scope `shouldBe` [StrLit "Hello"]
          blockResult scope `shouldBe` Nothing
        Left err -> fail $ "Parse failed: " ++ show err

    it "enforces block indentation" $ do
      let input = "block test:\n\"Hello\""  -- Not indented
      parseTextExpr (T.pack input) `shouldSatisfy` isLeft

    it "parses nested blocks" $ do
      let input = "block outer:\n  block inner:\n    \"Hello\""
      case parseTextExpr (T.pack input) of
        Right (Block outerScope) -> do
          blockLabel outerScope `shouldBe` "outer"
          case head (blockExprs outerScope) of
            Block innerScope -> do
              blockLabel innerScope `shouldBe` "inner"
              blockExprs innerScope `shouldBe` [StrLit "Hello"]
            _ -> fail "Expected inner block"
        Left err -> fail $ "Parse failed: " ++ show err

  describe "Break statement parsing" $ do
    it "parses break with label" $ do
      let input = "block test:\n  break test"
      case parseTextExpr (T.pack input) of
        Right (Block scope) -> do
          blockLabel scope `shouldBe` "test"
          blockExprs scope `shouldBe` [Break "test"]
        Left err -> fail $ "Parse failed: " ++ show err

    it "enforces break indentation" $ do
      let input = "block test:\nbreak test"  -- Not indented
      parseTextExpr (T.pack input) `shouldSatisfy` isLeft

  describe "Result handling" $ do
    it "parses result expression" $ do
      let input = "block test:\n  result \"Done\""
      case parseTextExpr (T.pack input) of
        Right (Block scope) -> do
          blockLabel scope `shouldBe` "test"
          blockResult scope `shouldBe` Just (Result (StrLit "Done"))
        Left err -> fail $ "Parse failed: " ++ show err

    it "allows only breaks after result" $ do
      let input = "block test:\n  result \"Done\"\n  \"Invalid\""
      parseTextExpr (T.pack input) `shouldSatisfy` isLeft

    it "parses result with following break" $ do
      let input = "block test:\n  result \"Done\"\n  break test"
      case parseTextExpr (T.pack input) of
        Right (Block scope) -> do
          blockLabel scope `shouldBe` "test"
          blockResult scope `shouldBe` Just (Result (StrLit "Done"))
          blockExprs scope `shouldBe` [Break "test"]
        Left err -> fail $ "Parse failed: " ++ show err

  describe "Program Parser" $ do
    it "parses hello world" $ do
      let input = "print \"Hello, World!\""
      case parseProgram (T.pack input) of
        Right (print, expr) -> do
          locToken print `shouldBe` TWord "print"
          locToken expr `shouldBe` TString "Hello, World!"
        Left err -> fail $ "Parse failed: " ++ show err

    it "fails on bad indentation" $ do
      let input = "  print \"Hello, World!\""  -- Indented when it shouldn't be
      parseProgram (T.pack input) `shouldSatisfy` isLeft

    it "requires strings to be properly indented" $ do
      let input = "print\n\"Hello\"" -- String on new line without proper indent
      parseProgram (T.pack input) `shouldSatisfy` isLeft
