{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zap.Codegen.CSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad (forM_)
import Control.Monad.State
import Control.Monad.Except
import Data.Either (isLeft)

import Zap.IR.Core
import Zap.Codegen.C

spec :: Spec
spec = do
  describe "C Code Generator" $ do
    describe "Basic Type Generation" $ do
      it "generates correct C types for numeric types" $ do
        let testCases =
              [ (IRTypeNum IRInt32, "int32_t")
              , (IRTypeNum IRInt64, "int64_t")
              , (IRTypeNum IRFloat32, "float")
              , (IRTypeNum IRFloat64, "double")
              ]
        forM_ testCases $ \(irType, expected) -> do
          evalState (runExceptT $ irTypeToCType irType) (CodegenState M.empty M.empty 0 [])
            `shouldBe` Right expected

      it "generates correct C types for vector types" $ do
        let testCases =
              [ (IRTypeVec (IRVec2 IRFloat32), "v2f32")
              , (IRTypeVec (IRVec3 IRFloat32), "v3f32")
              , (IRTypeVec (IRVec4 IRFloat32), "v4f32")
              , (IRTypeVec (IRVec2 IRFloat64), "v2f64")
              ]
        forM_ testCases $ \(irType, expected) -> do
          evalState (runExceptT $ irTypeToCType irType) (CodegenState M.empty M.empty 0 [])
            `shouldBe` Right expected

    describe "Struct Generation" $ do
      it "generates correct struct definitions" $ do
        let structDecl = IRStruct "Point"
              [ ("x", IRTypeNum IRFloat32)
              , ("y", IRTypeNum IRFloat32)
              ]
        let expected = T.unlines
              [ "typedef struct Point {"
              , "    float x;"
              , "    float y;"
              , "} Point_t;"
              ]
        evalState (runExceptT $ generateStructDef structDecl) (CodegenState M.empty M.empty 0 [])
          `shouldBe` Right expected

      it "handles nested struct types" $ do
        let lineDecl = IRStruct "Line"
              [ ("start", IRTypeStruct "Point" [])
              , ("end", IRTypeStruct "Point" [])
              ]
        let expected = T.unlines
              [ "typedef struct Line {"
              , "    Point_t start;"
              , "    Point_t end;"
              , "} Line_t;"
              ]
        evalState (runExceptT $ generateStructDef lineDecl) (CodegenState M.empty M.empty 0 [])
          `shouldBe` Right expected

    describe "Vector Operations" $ do
      it "generates SIMD code for vector addition" $ do
        let expr = IRBinOp IRAdd
              (IRVec (IRVec4 IRFloat32)
                [IRNum IRFloat32 "1.0", IRNum IRFloat32 "2.0",
                 IRNum IRFloat32 "3.0", IRNum IRFloat32 "4.0"])
              (IRVec (IRVec4 IRFloat32)
                [IRNum IRFloat32 "5.0", IRNum IRFloat32 "6.0",
                 IRNum IRFloat32 "7.0", IRNum IRFloat32 "8.0"])
        let result = evalState (runExceptT $ generateTypedExpr expr) (CodegenState M.empty M.empty 0 [])
        case result of
          Right (val, _) -> T.isInfixOf "_mm_add_ps" val `shouldBe` True
          Left err -> expectationFailure $ "Expected Right but got Left " ++ show err

    it "generates SIMD code for dot product" $ do
      let expr = IRBinOp IRDot
            (IRVec (IRVec4 IRFloat32)
              [IRNum IRFloat32 "1.0", IRNum IRFloat32 "2.0",
               IRNum IRFloat32 "3.0", IRNum IRFloat32 "4.0"])
            (IRVec (IRVec4 IRFloat32)
              [IRNum IRFloat32 "5.0", IRNum IRFloat32 "6.0",
               IRNum IRFloat32 "7.0", IRNum IRFloat32 "8.0"])
      let result = evalState (runExceptT $ generateTypedExpr expr) (CodegenState M.empty M.empty 0 [])
      case result of
        Right (val, _) -> T.isInfixOf "_mm_dp_ps" val `shouldBe` True
        Left err -> expectationFailure $ "Expected Right but got Left " ++ show err

    describe "Complete Program Generation" $ do
      it "generates a complete program with vector math" $ do
        let prog = IRProgram
              [ IRStruct "Vector4"
                  [ ("x", IRTypeNum IRFloat32)
                  , ("y", IRTypeNum IRFloat32)
                  , ("z", IRTypeNum IRFloat32)
                  , ("w", IRTypeNum IRFloat32)
                  ]
              , IRFunc "add_vectors"
                  [ ("v1", IRTypeVec (IRVec4 IRFloat32))
                  , ("v2", IRTypeVec (IRVec4 IRFloat32))
                  ]
                  (IRTypeVec (IRVec4 IRFloat32))
                  (IRBinOp IRAdd (IRVar "v1") (IRVar "v2"))
              ]
              [ IRPrint (IRString "Vector math test") ]

        let result = generateC prog
        result `shouldSatisfy` \case
          Right code -> T.isInfixOf "#include <immintrin.h>" code &&
                       T.isInfixOf "Vector4" code &&
                       T.isInfixOf "add_vectors" code
          Left _ -> False

    describe "Error Handling" $ do
      it "reports unsupported vector operations" $ do
        let expr = IRBinOp IRDot
              (IRVec (IRVec2 IRInt32) [IRNum IRInt32 "1", IRNum IRInt32 "2"])
              (IRVec (IRVec2 IRInt32) [IRNum IRInt32 "3", IRNum IRInt32 "4"])
        generateC (IRProgram [] [expr]) `shouldSatisfy` isLeft

      it "reports type mismatches in vector operations" $ do
        let expr = IRBinOp IRAdd
              (IRVec (IRVec4 IRFloat32)
                [IRNum IRFloat32 "1.0", IRNum IRFloat32 "2.0",
                 IRNum IRFloat32 "3.0", IRNum IRFloat32 "4.0"])
              (IRVec (IRVec3 IRFloat32)
                [IRNum IRFloat32 "5.0", IRNum IRFloat32 "6.0",
                 IRNum IRFloat32 "7.0"])
        generateC (IRProgram [] [expr]) `shouldSatisfy` isLeft

    describe "Function Call Generation" $ do
      it "generates code for simple function calls" $ do
        let expr = IRCall "add"
                    [ IRNum IRInt32 "1"
                    , IRNum IRInt32 "2"
                    ]
        let initialState = CodegenState
              { varEnv = M.empty
              , funcEnv = M.singleton "add" (IRTypeNum IRInt32, [IRTypeNum IRInt32, IRTypeNum IRInt32])
              , blockCounter = 0
              , structDefs = []
              }
        let result = evalState (runExceptT $ generateTypedExpr expr) initialState
        case result of
          Right (val, _) -> val `shouldBe` "add(1, 2)"
          Left err -> expectationFailure $ "Expected Right but got Left " ++ show err

      it "propagates correct return types for function calls" $ do
        let expr = IRCall "vector_add"
                    [ IRVec (IRVec4 IRFloat32)
                        [IRNum IRFloat32 "1.0", IRNum IRFloat32 "2.0",
                         IRNum IRFloat32 "3.0", IRNum IRFloat32 "4.0"]
                    , IRVec (IRVec4 IRFloat32)
                        [IRNum IRFloat32 "5.0", IRNum IRFloat32 "6.0",
                         IRNum IRFloat32 "7.0", IRNum IRFloat32 "8.0"]
                    ]
        let initialState = CodegenState
              { varEnv = M.empty
              , funcEnv = M.singleton "vector_add"
                  (IRTypeVec (IRVec4 IRFloat32),
                   [IRTypeVec (IRVec4 IRFloat32), IRTypeVec (IRVec4 IRFloat32)])
              , blockCounter = 0
              , structDefs = []
              }
        let result = evalState (runExceptT $ generateTypedExpr expr) initialState
        case result of
          Right (_, retType) -> retType `shouldBe` IRTypeVec (IRVec4 IRFloat32)
          Left err -> expectationFailure $ "Expected Right but got Left " ++ show err
