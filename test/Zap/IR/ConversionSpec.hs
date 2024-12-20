{-# LANGUAGE OverloadedStrings #-}
module Zap.IR.ConversionSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Control.Monad (forM_)

import Zap.AST
import Zap.IR.Core
import Zap.IR.Conversion

spec :: Spec
spec = do
  describe "IR Conversion" $ do
    describe "String Literals" $ do
      it "converts simple string literal" $ do
        let ast = Program [TLExpr (StrLit "Hello")]
        case convertToIR ast of
          Right (IRProgram decls exprs) -> do
            decls `shouldBe` []
            exprs `shouldBe` [IRString "Hello"]
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

    describe "Print Statements" $ do
      it "converts print with string literal" $ do
        let ast = Program [TLExpr (Print (StrLit "Hello"))]
        case convertToIR ast of
          Right (IRProgram decls exprs) -> do
            decls `shouldBe` []
            exprs `shouldBe` [IRPrint (IRString "Hello")]
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

    describe "Block Expressions" $ do
      it "converts simple block" $ do
        let ast = Program [TLExpr (Block $ BlockScope
              { blockLabel = "test"
              , blockExprs = [Print (StrLit "Inside")]
              , blockResult = Nothing
              })]
        case convertToIR ast of
          Right (IRProgram decls exprs) -> do
            decls `shouldBe` []
            case exprs of
              [IRBlockAlloc label bodyExprs result] -> do
                label `shouldBe` "test"
                bodyExprs `shouldBe` [IRPrint (IRString "Inside")]
                result `shouldBe` Nothing
              _ -> expectationFailure "Expected block allocation"
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

      it "converts block with result" $ do
        let ast = Program [TLExpr (Block $ BlockScope
              { blockLabel = "test"
              , blockExprs = []
              , blockResult = Just (Result (StrLit "Done"))
              })]
        case convertToIR ast of
          Right (IRProgram decls exprs) -> do
            decls `shouldBe` []
            case exprs of
              [IRBlockAlloc label bodyExprs (Just result)] -> do
                label `shouldBe` "test"
                bodyExprs `shouldBe` []
                result `shouldBe` IRResult (IRString "Done")
              _ -> expectationFailure "Expected block with result"
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

    describe "Type Conversion" $ do
      it "converts numeric types correctly" $ do
        let testCases =
              [ (TypeNum Int32, IRTypeNum IRInt32)
              , (TypeNum Int64, IRTypeNum IRInt64)
              , (TypeNum Float32, IRTypeNum IRFloat32)
              , (TypeNum Float64, IRTypeNum IRFloat64)
              ]
        forM_ testCases $ \(srcType, expectedType) -> do
          case convertType srcType of
            actualType -> actualType `shouldBe` expectedType

      it "converts vector types correctly" $ do
        let testCases =
              [ (TypeVec (Vec2 Float32), IRTypeVec (IRVec2 IRFloat32))
              , (TypeVec (Vec3 Float32), IRTypeVec (IRVec3 IRFloat32))
              , (TypeVec (Vec4 Float32), IRTypeVec (IRVec4 IRFloat32))
              ]
        forM_ testCases $ \(srcType, expectedType) -> do
          case convertType srcType of
            actualType -> actualType `shouldBe` expectedType

    describe "Error Handling" $ do
      it "reports errors for unsupported binary operations" $ do
        let ast = Program [TLExpr (BinOp Add (StrLit "a") (StrLit "b"))]
        case convertToIR ast of
          Left (InvalidType _) -> return ()
          Left err -> expectationFailure $ "Expected InvalidType but got: " ++ show err
          Right ir -> expectationFailure $ "Expected error but got successful conversion: " ++ show ir

      it "reports errors for unsupported variable references" $ do
        let ast = Program [TLExpr (Var "undefined_var")]
        case convertToIR ast of
          Left (UnknownVariable _) -> return ()
          Left err -> expectationFailure $ "Expected UnknownVariable but got: " ++ show err
          Right ir -> expectationFailure $ "Expected error but got successful conversion: " ++ show ir

      it "reports errors for invalid type combinations" $ do
        let ast = Program [TLExpr (Let "x" (BinOp Add (StrLit "str") (NumLit Int32 "42")))]
        case convertToIR ast of
          Left (InvalidType _) -> return ()
          Left err -> expectationFailure $ "Expected InvalidType but got: " ++ show err
          Right ir -> expectationFailure $ "Expected error but got successful conversion: " ++ show ir

      describe "Binary Operations" $ do
        it "converts simple numeric addition" $ do
          let ast = Program [TLExpr (BinOp Add
                                    (NumLit Int32 "1")
                                    (NumLit Int32 "2"))]
          case convertToIR ast of
            Right (IRProgram decls exprs) -> do
              decls `shouldBe` []
              exprs `shouldBe` [IRBinOp IRAdd
                               (IRNum IRInt32 "1")
                               (IRNum IRInt32 "2")]
            Left err -> expectationFailure $ "Conversion failed: " ++ show err

        it "converts simple numeric subtraction" $ do
          let ast = Program [TLExpr (BinOp Sub
                                    (NumLit Int32 "5")
                                    (NumLit Int32 "3"))]
          case convertToIR ast of
            Right (IRProgram decls exprs) -> do
              decls `shouldBe` []
              exprs `shouldBe` [IRBinOp IRSub
                               (IRNum IRInt32 "5")
                               (IRNum IRInt32 "3")]
            Left err -> expectationFailure $ "Conversion failed: " ++ show err

        it "converts simple numeric multiplication" $ do
          let ast = Program [TLExpr (BinOp Mul
                                    (NumLit Int32 "4")
                                    (NumLit Int32 "3"))]
          case convertToIR ast of
            Right (IRProgram decls exprs) -> do
              decls `shouldBe` []
              exprs `shouldBe` [IRBinOp IRMul
                               (IRNum IRInt32 "4")
                               (IRNum IRInt32 "3")]
            Left err -> expectationFailure $ "Conversion failed: " ++ show err

        it "converts simple numeric division" $ do
          let ast = Program [TLExpr (BinOp Div
                                    (NumLit Int32 "6")
                                    (NumLit Int32 "2"))]
          case convertToIR ast of
            Right (IRProgram decls exprs) -> do
              decls `shouldBe` []
              exprs `shouldBe` [IRBinOp IRDiv
                               (IRNum IRInt32 "6")
                               (IRNum IRInt32 "2")]
            Left err -> expectationFailure $ "Conversion failed: " ++ show err

      describe "Control Flow" $ do
        describe "If Expressions" $ do
          it "converts simple if-else with boolean condition" $ do
            let ast = Program [TLExpr (If
                                      (BoolLit True)
                                      (NumLit Int32 "1")
                                      (NumLit Int32 "2"))]
            case convertToIR ast of
              Right (IRProgram decls exprs) -> do
                decls `shouldBe` []
                exprs `shouldBe` [IRIf
                                 (IRBool True)
                                 (IRNum IRInt32 "1")
                                 (IRNum IRInt32 "2")]
              Left err -> expectationFailure $ "Conversion failed: " ++ show err
