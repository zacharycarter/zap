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
    it "parses traditional print syntax" $ do
      let input = "print \"Hello, World!\""
      case parseProgram input of
        Right [TLExpr (Call "print" [StrLit str])] ->
          str `shouldBe` "Hello, World!"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "parses function-style print syntax" $ do
      let input = "print(\"Hello, World!\")"
      case parseProgram input of
        Right [TLExpr (Call "print" [StrLit str])] ->
          str `shouldBe` "Hello, World!"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "parses print with complex expression" $ do
      parseProgram "print(1 + 2 * 3)" `shouldBe`
        Right [TLExpr (Call "print" [
            BinOp Add
                (NumLit Int32 "1")
                (BinOp Mul (NumLit Int32 "2") (NumLit Int32 "3"))])]

    it "enforces print statement indentation" $ do
      let input = "block test:\nprint \"Hello\""  -- Not indented
      parseProgram input `shouldSatisfy` isLeft

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
        Right [TLExpr (Call "print" [StrLit first]), TLExpr (Block scope)] -> do
          first `shouldBe` "First"
          blockExprs scope `shouldBe` [Call "print" [StrLit "Second"]]
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

  describe "While loop parsing" $ do
    it "parses simple while loop" $ do
      let input = "while x < 3:\n  print x"
      case parseProgram input of
        Right [TLExpr (While (BinOp Lt (Var "x") (NumLit Int32 "3")) (Block scope))] ->
          blockExprs scope `shouldBe` [Call "print" [Var "x"]]
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other


    it "enforces proper while loop structure" $ do
      let input = "while x < 3"  -- Missing colon
      parseProgram input `shouldSatisfy` isLeft

  describe "Numeric literal parsing" $ do
    it "parses integer literals as Int32" $ do
      parseExprFromText "42" `shouldBe` Right (NumLit Int32 "42")

    it "parses decimal literals as Float32" $ do
      parseExprFromText "42.0" `shouldBe` Right (NumLit Float32 "42.0")

  describe "Variable declarations and assignments" $ do
    it "parses variable declaration with initialization" $ do
        parseExprFromText "var x = 42" `shouldBe`
            Right (VarDecl "x" (NumLit Int32 "42"))

    it "parses variable declaration within block" $ do
        parseProgram "block test:\n  var x = 42" `shouldBe`
            Right [TLExpr (Block $ BlockScope
                "test"
                [VarDecl "x" (NumLit Int32 "42")]
                Nothing)]

    it "parses variable assignment" $ do
        parseExprFromText "x = 42" `shouldBe`
            Right (Assign "x" (NumLit Int32 "42"))

    it "parses variable declaration and assignment in function" $ do
        let input = T.unlines
              [ "fn sum_squares(x, y: i32): i32 ="
              , "  var sum = x * x"
              , "  sum = sum + y * y"
              , "  sum"
              ]
        parseProgram input `shouldBe`
            Right [TLDecl (DFunc "sum_squares"
                [Param "x" (TypeNum Int32), Param "y" (TypeNum Int32)]
                (TypeNum Int32)
                (Block $ BlockScope "function_body"
                    [ VarDecl "sum" (BinOp Mul (Var "x") (Var "x"))
                    , Assign "sum" (BinOp Add (Var "sum")
                        (BinOp Mul (Var "y") (Var "y")))
                    , Var "sum"
                    ]
                    Nothing))]
