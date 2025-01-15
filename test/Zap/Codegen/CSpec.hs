{-# LANGUAGE OverloadedStrings #-}

module Zap.Codegen.CSpec (spec) where

import Test.Hspec
import qualified Data.Set as S
import qualified Data.Text as T

import Zap.AST
import Zap.IR
import Zap.Codegen.C

spec :: Spec
spec = do
  describe "C Code Generation" $ do
    describe "Literal code generation" $ do
      it "generates valid C numeric literals" $ do
          let program = IRProgram
                [ ( IRFuncDecl "main" [] IRTypeVoid
                    (IRBlock "entry"
                      [ (IRVarDecl "x" IRTypeInt32 (IRLit (IRInt32Lit 5)), testMeta)
                      , (IRAssign "x" (IRLit (IRInt32Lit 3)), testMeta)
                      ])
                  , testMeta)
                ]
          case generateC program of
              Right code -> do
                  T.isInfixOf "int32_t x = 5" code `shouldBe` True
                  T.isInfixOf "x = 3" code `shouldBe` True
              Left err -> expectationFailure $ show err

      it "generates struct constructor call" $ do
        let struct_call = IRCall "struct_lit"
              [ IRLit (IRStringLit "Point")
              , IRLit (IRInt32Lit 10)
              , IRLit (IRInt32Lit 20)
              ]
        let program = IRProgram [(IRFuncDecl "main" [] IRTypeVoid
              (IRBlock "entry" [(IRStmtExpr struct_call, testMeta)]), testMeta)]

        case generateC program of
            Right code -> do
                T.isInfixOf "(struct Point) {10, 20}" code `shouldBe` True
            Left err -> expectationFailure $ show err

      it "preserves field names in struct definitions" $ do
          let symTable = snd $ registerStruct "Point"
                                [("x", TypeNum Int32), ("y", TypeNum Int32)]
                                emptySymbolTable
          let pointDecl = IRVarDecl "p"
                (IRTypeStruct "Point" (StructId 0))
                (IRCall "struct_lit"
                  [IRLit (IRStringLit "Point"),
                   IRLit (IRInt32Lit 10),
                   IRLit (IRInt32Lit 20)])
          let testMetaWithStruct = testMeta { metaSymTable = Just symTable }
          let program = IRProgram
                [(IRFuncDecl "main" [] IRTypeVoid
                  (IRBlock "entry" [(pointDecl, testMetaWithStruct)]), testMetaWithStruct)]

          case generateC program of
            Right code -> do
              T.isInfixOf "struct Point {" code `shouldBe` True
              T.isInfixOf "int32_t x;" code `shouldBe` True
              T.isInfixOf "int32_t y;" code `shouldBe` True
            Left err -> expectationFailure $ show err

      it "generates non-generic struct definitions correctly" $ do
          let symTable = snd $ registerStruct "Point"
                                [("x", TypeNum Int32), ("y", TypeNum Int32)]
                                emptySymbolTable
          let pointDecl = IRVarDecl "p"
                (IRTypeStruct "Point" (StructId 0))
                (IRCall "struct_lit"
                  [IRLit (IRStringLit "Point"),
                   IRLit (IRInt32Lit 10),
                   IRLit (IRInt32Lit 20)])
          let testMetaWithStruct = testMeta { metaSymTable = Just symTable }
          let program = IRProgram
                [(IRFuncDecl "main" [] IRTypeVoid
                  (IRBlock "entry" [(pointDecl, testMetaWithStruct)]), testMetaWithStruct)]

          case generateC program of
            Right code -> do
              T.isInfixOf "struct Point {" code `shouldBe` True
              T.count "struct Point {\n    int32_t x;\n    int32_t y;\n};" code `shouldBe` 1  -- Only one definition
              T.isInfixOf "int32_t x;" code `shouldBe` True
              T.isInfixOf "int32_t y;" code `shouldBe` True
            Left err -> expectationFailure $ show err

    describe "Label generation" $ do
      it "only generates used labels in control flow" $ do
        let input = IRProgram
              [ ( IRFuncDecl "main" [] IRTypeVoid
                  (IRBlock "entry"
                    [ (IRLabel "start", testMeta)
                    , (IRJumpIfZero (IRLit (IRBoolLit True)) "exit", testMeta)  -- Only uses "exit"
                    , (IRLabel "unused", testMeta)                               -- Never referenced
                    , (IRLabel "exit", testMeta)
                    , (IRReturn Nothing, testMeta)
                    ])
                , testMeta)
              ]

        case generateC input of
          Right code -> do
            -- The generated code shows we're doing the right thing:
            -- 1. Only "exit:" label appears
            -- 2. "start:" and "unused:" are skipped
            T.isInfixOf "exit:" code `shouldBe` True     -- Used in jump, should be present
            T.isInfixOf "start:" code `shouldBe` False   -- Never used, should be absent
            T.isInfixOf "unused:" code `shouldBe` False  -- Never used, should be absent

          Left err ->
            expectationFailure $ "Code generation failed: " ++ show err
           
      where
        testMeta = IRMetadata
          { metaType = IRTypeVoid
          , metaEffects = S.singleton PureEffect
          , metaSourcePos = Nothing
          , metaLiteralType = Nothing
          , metaSymTable = Nothing
          }
