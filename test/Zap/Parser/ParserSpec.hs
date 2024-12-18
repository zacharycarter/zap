{-# LANGUAGE OverloadedStrings #-}
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

spec :: Spec
spec = do
  describe "Print statement parsing" $ do
    it "parses hello world" $ do
      let input = "print \"Hello, World!\""
      case parseProgram input of
        Right [TLExpr (Print (StrLit str))] ->
          str `shouldBe` "Hello, World!"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

  describe "Block parsing" $ do
    it "parses simple block" $ do
      let input = "block test:\n  \"Hello\""
      case parseProgram input of
        Right [TLExpr (Block scope)] -> do
          blockLabel scope `shouldBe` "test"
          blockExprs scope `shouldBe` [StrLit "Hello"]
          blockResult scope `shouldBe` Nothing
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "enforces block indentation" $ do
      let input = "block test:\n\"Hello\""  -- Not indented
      parseProgram input `shouldSatisfy` isLeft

    it "parses nested blocks" $ do
      let input = "block outer:\n  block inner:\n    \"Hello\""
      case parseProgram input of
        Right [TLExpr (Block outerScope)] -> do
          blockLabel outerScope `shouldBe` "outer"
          case head (blockExprs outerScope) of
            Block innerScope -> do
              blockLabel innerScope `shouldBe` "inner"
              blockExprs innerScope `shouldBe` [StrLit "Hello"]
            _ -> expectationFailure "Expected inner block"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    -- Rest of the test cases follow similar pattern...

  describe "Top-level expressions" $ do
    it "parses multiple top-level expressions" $ do
      let input = T.unlines
            [ "print \"First\""
            , "block test:"
            , "  print \"Second\""
            ]
      case parseProgram input of
        Right [TLExpr (Print (StrLit first)), TLExpr (Block scope)] -> do
          first `shouldBe` "First"
          blockExprs scope `shouldBe` [Print (StrLit "Second")]
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other
