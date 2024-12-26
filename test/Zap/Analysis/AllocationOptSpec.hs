{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.AllocationOptSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Data.List (sort)
import Debug.Trace

import Zap.IR.Core
import Zap.Analysis.AllocationOpt
import Zap.Util (mkTestExpr)

spec :: Spec
spec = describe "Allocation Optimization" $ do
  it "converts heap allocations to stack when possible" $ do
    let ir = IRProgram []
              [mkTestExpr $ IRLetAlloc "x" (mkTestExpr $ IRNum IRInt32 "42") IRAllocHeap]
    let expected = IRProgram []
                    [mkTestExpr $ IRLetAlloc "x" (mkTestExpr $ IRNum IRInt32 "42") IRAllocStack]
    case optimizeAllocations ir of
      Right (optimized, stats) -> do
        optimized `shouldBe` expected
        stackSavings stats `shouldSatisfy` (> 0)
      Left err -> expectationFailure $ "Optimization failed: " ++ show err

  it "preserves heap allocations for large objects" $ do
    let fields = [(T.pack $ show i, mkTestExpr $ IRNum IRInt32 "0") | i <- [(1 :: Integer)..(100 :: Integer)]]
    let ir = IRProgram []
              [mkTestExpr $ IRLetAlloc "x" (mkTestExpr $ IRStructLit "LargeStruct" fields) IRAllocHeap]
    case optimizeAllocations ir of
      Right (optimized, _) ->
        optimized `shouldBe` ir
      Left err -> expectationFailure $ "Optimization failed: " ++ show err

  it "aligns vector allocations for SIMD" $ do
    let vecElements = replicate 4 (mkTestExpr $ IRNum IRFloat32 "1.0")
    let ir = IRProgram []
              [mkTestExpr $ IRLetAlloc "v"
                (mkTestExpr $ IRVec (IRVec4 IRFloat32) vecElements)
                IRAllocDefault]
    case optimizeAllocations ir of
      Right (optimized, stats) -> do
        simdOptimizations stats `shouldBe` 1
        checkVectorAlignment optimized
      Left err -> expectationFailure $ "Optimization failed: " ++ show err

  it "reorders parameters for better packing" $ do
    let func = IRFunc "test"
                [ ("small", IRTypeNum IRInt32)
                , ("large", IRTypeVec (IRVec4 IRFloat32))
                , ("med", IRTypeNum IRInt64)]
                IRTypeVoid
                (mkTestExpr $ IRVar "small")
    let ir = IRProgram [func] []
    case optimizeAllocations ir of
      Right (IRProgram [optimized] [], stats) -> do
        parameterReordering stats `shouldBe` 1
        checkParameterOrder optimized
      Right r -> expectationFailure $ "Optimization failed, unexpected results: " ++ show r
      Left err -> expectationFailure $ "Optimization failed: " ++ show err

checkVectorAlignment :: IR -> Expectation
checkVectorAlignment (IRProgram _ [IRExpr _ (IRLetAlloc _ _ strat)]) =
  case strat of
    IRAllocStack -> pure () -- Stack allocations are aligned by default
    other -> expectationFailure $ "Expected stack allocation, got: " ++ show other
checkVectorAlignment other =
  expectationFailure $ "Unexpected optimization result: " ++ show other

checkParameterOrder :: IRDecl -> Expectation
checkParameterOrder (IRFunc _ params _ _) = do
  traceM $ "checking parameter order for params: " ++ show params
  -- Verify that vector parameters come first (for alignment)
  let (vectors, rest) = span (isVectorType . snd) params
  not (null vectors) `shouldBe` True
  -- Verify remaining parameters are ordered by decreasing size
  let sizes = map (typeSizeBytes . snd) rest
  sizes `shouldBe` reverse (sort sizes)
checkParameterOrder other =
  expectationFailure $ "Expected function, got: " ++ show other

typeSizeBytes :: IRType -> Int
typeSizeBytes (IRTypeNum IRInt32) = 4
typeSizeBytes (IRTypeNum IRInt64) = 8
typeSizeBytes (IRTypeNum IRFloat32) = 4
typeSizeBytes (IRTypeNum IRFloat64) = 8
typeSizeBytes (IRTypeVec (IRVec2 _)) = 8
typeSizeBytes (IRTypeVec (IRVec3 _)) = 12
typeSizeBytes (IRTypeVec (IRVec4 _)) = 16
typeSizeBytes _ = 0
