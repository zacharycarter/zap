{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.AllocationSpec (spec) where

import Test.Hspec
import Data.Either (isLeft, isRight)

import Zap.IR.Core
import Zap.Analysis.Allocation
import Zap.Util (mkTestExpr)


spec :: Spec
spec = do
    describe "Basic Allocations" $ do
        it "allows valid stack allocations" $ do
            let ex = mkTestExpr $ IRVarAlloc "x" IRAllocStack
            analyzeAllocations (IRProgram [] [ex]) `shouldBe` Right (IRProgram [] [ex])

        it "prevents invalid stack allocations" $ do
            let ex = mkTestExpr $ IRLetAlloc "x" (mkTestExpr $ IRString "hello") IRAllocStack
            analyzeAllocations (IRProgram [] [ex]) `shouldSatisfy` isLeft

    describe "Arena Allocations" $ do
        it "detects invalid arena usage" $ do
            let ex = mkTestExpr $ IRVarAlloc "x" IRAllocArena
            analyzeAllocations (IRProgram [] [ex]) `shouldSatisfy` isLeft

        it "allows valid arena allocations" $ do
            let setup = mkTestExpr $ IRBlockAlloc "arena" [] Nothing
                alloc = mkTestExpr $ IRVarAlloc "x" IRAllocArena
            analyzeAllocations (IRProgram [] [setup, alloc]) `shouldSatisfy` isRight

    describe "Memory Leak Detection" $ do
        it "detects potential heap leaks" $ do
            let alloc = mkTestExpr $ IRLetAlloc "x" (mkTestExpr $ IRString "leak") IRAllocHeap
            analyzeAllocations (IRProgram [] [alloc]) `shouldSatisfy` isLeft

        it "allows properly freed allocations" $ do
            let alloc = mkTestExpr $ IRLetAlloc "x" (mkTestExpr $ IRString "temp") IRAllocTemp
                free = mkTestExpr $ IRBlockAlloc "scope" [alloc] Nothing
            analyzeAllocations (IRProgram [] [free]) `shouldSatisfy` isRight

    describe "Scope Analysis" $ do
        it "tracks allocations in correct scope" $ do
            let outer = mkTestExpr $ IRBlockAlloc "outer"
                        [mkTestExpr $ IRVarAlloc "x" IRAllocStack]
                        Nothing
                inner = mkTestExpr $ IRBlockAlloc "inner"
                        [mkTestExpr $ IRVarAlloc "y" IRAllocStack]
                        Nothing
            analyzeAllocations (IRProgram [] [outer, inner]) `shouldSatisfy` isRight

        it "prevents escaping temporaries" $ do
            let temp = mkTestExpr $ IRVarAlloc "temp" IRAllocTemp
                escape = mkTestExpr $ IRLetAlloc "escape" (mkTestExpr $ IRVar "temp") IRAllocHeap
            analyzeAllocations (IRProgram [] [temp, escape]) `shouldSatisfy` isLeft

    describe "Function Analysis" $ do
        it "validates numeric parameters" $ do
            let func = IRFunc "test"
                    [("x", IRTypeNum IRInt32), ("y", IRTypeNum IRFloat64)]
                    (IRTypeNum IRInt32)
                    (mkTestExpr $ IRVar "x")
            analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

        it "validates boolean parameters" $ do
            let func = IRFunc "test"
                    [("flag", IRTypeBool)]
                    IRTypeBool
                    (mkTestExpr $ IRVar "flag")
            analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

        it "validates string parameters" $ do
            let func = IRFunc "test"
                    [("str", IRTypeString)]
                    IRTypeString
                    (mkTestExpr $ IRVar "str")
            analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight
