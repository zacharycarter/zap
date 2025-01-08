{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.SemanticSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import Debug.Trace

import Zap.AST
import Zap.Analysis.Semantic

spec :: Spec
spec = do
  describe "Basic Semantic Analysis" $ do
    describe "Variable Scoping" $ do
      it "detects undefined variables" $ do
        let ast = Program [TLExpr $ Var "x"]
        analyze ast `shouldBe` Left (UndefinedVariable "x")

      it "allows access to variables after declaration" $ do
        let ast = Program
              [ TLExpr $ Let "x" (NumLit Int32 "1")
              , TLExpr $ Var "x"
              ]
        analyze ast `shouldBe` Right ast

      it "maintains proper function parameter scope" $ do
        let ast = Program
              [ TLDecl $ DFunc "f"
                  [Param "x" (TypeNum Int32)]
                  (TypeNum Int32)
                  (Var "x")
              ]
        analyze ast `shouldBe` Right ast

      it "prevents access to function parameters outside function" $ do
        let ast = Program
              [ TLDecl $ DFunc "f"
                  [Param "x" (TypeNum Int32)]
                  (TypeNum Int32)
                  (Var "x")
              , TLExpr $ Var "x"  -- Should fail
              ]
        analyze ast `shouldBe` Left (UndefinedVariable "x")

    describe "Function Declarations" $ do
      it "detects undefined functions" $ do
        let ast = Program [TLExpr $ Call "undefined" []]
        analyze ast `shouldBe` Left (UndefinedFunction "undefined")

      it "validates function arity" $ do
        let ast = Program
              [ TLDecl $ DFunc "f"
                  [Param "x" (TypeNum Int32)]
                  (TypeNum Int32)
                  (Var "x")
              , TLExpr $ Call "f" []  -- Wrong number of arguments
              ]
        analyze ast `shouldBe` Left (ArgumentCountMismatch "f" 1 0)

      it "prevents duplicate function definitions" $ do
        let ast = Program
              [ TLDecl $ DFunc "f" [] TypeBool (BoolLit True)
              , TLDecl $ DFunc "f" [] TypeBool (BoolLit False)
              ]
        analyze ast `shouldBe` Left (RecursionInGlobalScope "f")

    describe "String Literals" $ do
      it "rejects empty string literals" $ do
        let ast = Program [TLExpr $ StrLit ""]
        analyze ast `shouldBe` Left EmptyStringLiteral

      it "accepts non-empty string literals" $ do
        let ast = Program [TLExpr $ StrLit "hello"]
        analyze ast `shouldBe` Right ast

    describe "Field Access" $ do
      it "allows valid field access on vectors" $ do
        let ast = Program
              [ TLExpr $ FieldAccess
                  (Call "Vec2"
                    [NumLit Float32 "1.0", NumLit Float32 "2.0"])
                  "x"
              ]
        analyze ast `shouldBe` Right ast

      it "allows chained field access" $ do
        let ast = Program
              [ TLExpr $ FieldAccess
                  (FieldAccess
                    (Call "Vec3"
                      [ NumLit Float32 "1.0"
                      , NumLit Float32 "2.0"
                      , NumLit Float32 "3.0"
                      ])
                    "x")
                  "y"
              ]
        analyze ast `shouldBe` Right ast

    describe "Built-in Functions" $ do
      it "allows print with any argument" $ do
        let ast = Program [TLExpr $ Call "print" [StrLit "hello"]]
        analyze ast `shouldBe` Right ast

      it "validates print arity" $ do
        let ast = Program [TLExpr $ Call "print" []]
        analyze ast `shouldBe` Left (ArgumentCountMismatch "print" 1 0)
