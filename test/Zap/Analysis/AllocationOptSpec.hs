{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.AllocationOptSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except
import Data.List (sort)
import Debug.Trace

import Zap.IR.Core
import Zap.Analysis.AllocationOpt
import Zap.Analysis.Allocation (AllocError(..))
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
    let fields = [(T.pack $ show i, mkTestExpr $ IRNum IRInt32 "0") | i <- [1..100]]
    let ir = IRProgram []
              [mkTestExpr $ IRLetAlloc "x" (mkTestExpr $ IRStructLit "LargeStruct" fields) IRAllocHeap]
    let Right (optimized, _) = optimizeAllocations ir
    optimized `shouldBe` ir

  it "aligns vector allocations for SIMD" $ do
    let vecElements = replicate 4 (mkTestExpr $ IRNum IRFloat32 "1.0")
    let ir = IRProgram []
              [mkTestExpr $ IRLetAlloc "v"
                (mkTestExpr $ IRVec (IRVec4 IRFloat32) vecElements)
                IRAllocDefault]
    let Right (optimized, stats) = optimizeAllocations ir
    simdOptimizations stats `shouldBe` 1
    checkVectorAlignment optimized

  it "reorders parameters for better packing" $ do
    let func = IRFunc "test"
                [ ("small", IRTypeNum IRInt32)
                , ("large", IRTypeVec (IRVec4 IRFloat32))
                , ("med", IRTypeNum IRInt64)]
                IRTypeVoid
                (mkTestExpr $ IRVar "small")
    let ir = IRProgram [func] []
    let Right (IRProgram [optimized] [], stats) = optimizeAllocations ir
    parameterReordering stats `shouldBe` 1
    checkParameterOrder optimized

shouldBePositive :: Integer -> Expectation
shouldBePositive x = x `shouldSatisfy` (> 0)

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
