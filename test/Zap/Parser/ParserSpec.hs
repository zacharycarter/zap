{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.ParserSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)
import qualified Data.Text as T

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
                (NumLit Int64 "1")
                (BinOp Mul (NumLit Int64 "2") (NumLit Int64 "3"))])]

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
          case blockExprs outerScope of
            (Block innerScope:_) -> do
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
        Right [TLExpr (While (BinOp Lt (Var "x") (NumLit Int64"3")) (Block scope))] ->
          blockExprs scope `shouldBe` [Call "print" [Var "x"]]
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other


    it "enforces proper while loop structure" $ do
      let input = "while x < 3"  -- Missing colon
      parseProgram input `shouldSatisfy` isLeft

  describe "Numeric literal parsing" $ do
    it "parses integer literals as Int64" $ do
      parseExprFromText "42" `shouldBe` Right (NumLit Int64 "42")

    it "parses decimal literals as Float64" $ do
      parseExprFromText "42.0" `shouldBe` Right (NumLit Float64 "42.0")

  describe "Variable declarations and assignments" $ do
    it "parses variable declaration with initialization" $ do
        parseExprFromText "var x = 42" `shouldBe`
            Right (VarDecl "x" (NumLit Int64"42"))

    it "parses variable declaration within block" $ do
        parseProgram "block test:\n  var x = 42" `shouldBe`
            Right [TLExpr (Block $ BlockScope
                "test"
                [VarDecl "x" (NumLit Int64 "42")]
                Nothing)]

    it "parses variable assignment" $ do
        parseExprFromText "x = 42" `shouldBe`
            Right (Assign "x" (NumLit Int64 "42"))

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
                (Block $ BlockScope
                    { blockLabel = "function_body"
                    , blockExprs = [
                        VarDecl "sum" (BinOp Mul (Var "x") (Var "x")),
                        Assign "sum" (BinOp Add (Var "sum") (BinOp Mul (Var "y") (Var "y")))
                        ]
                    , blockResult = Just (Var "sum")
                    }))]

  describe "Type consistency in numeric literals" $ do
    it "maintains consistent type information between literal and declared type" $ do
      let input = "let x: i32 = 42"  -- Variable with explicit type annotation
      case parseProgram input of
        Right [TLExpr (Let "x" expr)] -> do
          -- The NumLit type should match the declared type
          case expr of
            NumLit numType val -> do
              numType `shouldBe` Int32
              TypeNum numType `shouldBe` TypeNum Int32
            _ -> expectationFailure "Expected numeric literal"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "infers consistent types for numeric literals" $ do
      let input = "42'i32"  -- Literal with type suffix
      case parseExprFromText input of
        Right (NumLit numType val) -> do
          numType `shouldBe` Int32
          val `shouldBe` "42"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "handles literals with contextual type information" $ do
      let input = "let x: i32 = 42\nlet y: i64 = x"
      case parseProgram input of
        Right [first, second] -> do
          case first of
            TLExpr (Let "x" (NumLit Int32 "42")) -> return ()  -- Accept old form
            TLExpr (Let "x" (Lit (IntLit "42"))) -> return ()  -- Accept new form
            other -> expectationFailure $ "Unexpected first expr: " ++ show other
          second `shouldBe` TLExpr (Let "y" (Var "x"))
        other -> expectationFailure $ "Unexpected parse result: " ++ show other

    it "enforces type consistency with float literals" $ do
      let input = "let x: f32 = 3.14"
      case parseProgram input of
        Right [TLExpr (Let "x" (Lit (FloatLit "3.14")))] -> return ()
        other -> expectationFailure $ "Unexpected parse result: " ++ show other
