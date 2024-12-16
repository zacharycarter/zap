{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.AllocationSpec (spec) where

import Test.Hspec
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Except

import Zap.IR.Core
import Zap.Analysis.Allocation

spec :: Spec
spec = do
  describe "Allocation Analysis" $ do
    describe "Basic Allocations" $ do
      it "allows valid stack allocations" $ do
        let expr = IRVarAlloc "x" IRAllocStack
        analyzeAllocations (IRProgram [] [expr]) `shouldBe` Right (IRProgram [] [expr])

      it "prevents invalid stack allocations" $ do
        let expr = IRLetAlloc "x" (IRString "hello") IRAllocStack
        analyzeAllocations (IRProgram [] [expr]) `shouldSatisfy` isLeft

    describe "Arena Allocations" $ do
      it "detects invalid arena usage" $ do
        let expr = IRVarAlloc "x" IRAllocArena
        analyzeAllocations (IRProgram [] [expr]) `shouldSatisfy` isLeft

      it "allows valid arena allocations" $ do
        let setup = IRBlockAlloc "arena" [] Nothing
            alloc = IRVarAlloc "x" IRAllocArena
        analyzeAllocations (IRProgram [] [setup, alloc]) `shouldSatisfy` isRight

    describe "Memory Leak Detection" $ do
      it "detects potential heap leaks" $ do
        let alloc = IRLetAlloc "x" (IRString "leak") IRAllocHeap
        analyzeAllocations (IRProgram [] [alloc]) `shouldSatisfy` isLeft

      it "allows properly freed allocations" $ do
        let alloc = IRLetAlloc "x" (IRString "temp") IRAllocTemp
            free = IRBlockAlloc "scope" [alloc] Nothing
        analyzeAllocations (IRProgram [] [free]) `shouldSatisfy` isRight

    describe "Scope Analysis" $ do
      it "tracks allocations in correct scope" $ do
        let outer = IRBlockAlloc "outer"
              [IRVarAlloc "x" IRAllocStack]
              Nothing
            inner = IRBlockAlloc "inner"
              [IRVarAlloc "y" IRAllocStack]
              Nothing
        analyzeAllocations (IRProgram [] [outer, inner]) `shouldSatisfy` isRight

      it "prevents escaping temporaries" $ do
        let temp = IRVarAlloc "temp" IRAllocTemp
            escape = IRLetAlloc "escape" (IRVar "temp") IRAllocHeap
        analyzeAllocations (IRProgram [] [temp, escape]) `shouldSatisfy` isLeft

    describe "Custom Allocators" $ do
      it "validates custom allocator usage" $ do
        let custom = IRVarAlloc "x" (IRAllocCustom "myalloc")
        analyzeAllocations (IRProgram [] [custom]) `shouldSatisfy` isLeft

      it "allows registered custom allocators" $ do
        let register = IRBlockAlloc "reg"
              [IRVarAlloc "myalloc" IRAllocHeap]
              Nothing
            use = IRVarAlloc "x" (IRAllocCustom "myalloc")
        analyzeAllocations (IRProgram [] [register, use]) `shouldSatisfy` isRight

  describe "Parameter Allocation Analysis" $ do
    describe "Basic Parameter Types" $ do
      it "validates simple numeric parameters" $ do
        let func = IRFunc "test"
              [("x", IRTypeNum IRInt32), ("y", IRTypeNum IRFloat64)]
              (IRTypeNum IRInt32)
              (IRVar "x")
        analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

      it "validates boolean parameters" $ do
        let func = IRFunc "test"
              [("flag", IRTypeBool)]
              IRTypeBool
              (IRVar "flag")
        analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

      it "validates string parameters" $ do
        let func = IRFunc "test"
              [("str", IRTypeString)]
              IRTypeString
              (IRVar "str")
        analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

    describe "Compound Parameter Types" $ do
      it "validates vector parameters" $ do
        let func = IRFunc "test"
              [("vec", IRTypeVec (IRVec4 IRFloat32))]
              (IRTypeVec (IRVec4 IRFloat32))
              (IRVar "vec")
        analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

      it "validates struct parameters" $ do
        let structDef = IRStruct "Point"
              [("x", IRTypeNum IRFloat32), ("y", IRTypeNum IRFloat32)]
        let func = IRFunc "test"
              [("pt", IRTypeStruct "Point" [])]
              (IRTypeStruct "Point" [])
              (IRVar "pt")
        analyzeAllocations (IRProgram [structDef, func] []) `shouldSatisfy` isRight

      it "detects circular struct references" $ do
        let structA = IRStruct "A" [("b", IRTypeStruct "B" [])]
        let structB = IRStruct "B" [("a", IRTypeStruct "A" [])]
        let func = IRFunc "test"
              [("a", IRTypeStruct "A" [])]
              (IRTypeStruct "A" [])
              (IRVar "a")
        analyzeAllocations (IRProgram [structA, structB, func] []) `shouldSatisfy` isLeft

    describe "Array Parameters" $ do
      it "validates array parameters" $ do
        let func = IRFunc "test"
              [("arr", IRTypeArray (IRTypeNum IRInt32))]
              (IRTypeArray (IRTypeNum IRInt32))
              (IRVar "arr")
        analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

      it "tracks array parameter lifetimes" $ do
        let func = IRFunc "test"
              [("arr", IRTypeArray (IRTypeNum IRInt32))]
              (IRTypeNum IRInt32)
              (IRIndex (IRVar "arr") (IRNum IRInt32 "0"))
        analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

    describe "SIMD Alignment" $ do
      it "ensures proper vector alignment" $ do
        let func = IRFunc "test"
              [("v1", IRTypeVec (IRVec4 IRFloat32)),
               ("v2", IRTypeVec (IRVec4 IRFloat32))]
              (IRTypeVec (IRVec4 IRFloat32))
              (IRBinOp IRAdd (IRVar "v1") (IRVar "v2"))
        analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

    describe "Multiple Parameters" $ do
      it "handles functions with many parameters" $ do
        let func = IRFunc "test"
              [("i", IRTypeNum IRInt32),
               ("f", IRTypeNum IRFloat64),
               ("b", IRTypeBool),
               ("s", IRTypeString),
               ("v", IRTypeVec (IRVec4 IRFloat32))]
              (IRTypeNum IRInt32)
              (IRVar "i")
        analyzeAllocations (IRProgram [func] []) `shouldSatisfy` isRight

      it "validates parameter type combinations" $ do
        let structDef = IRStruct "Complex"
              [("real", IRTypeNum IRFloat64),
               ("imag", IRTypeNum IRFloat64)]
        let func = IRFunc "test"
              [("n", IRTypeNum IRInt32),
               ("c", IRTypeStruct "Complex" []),
               ("arr", IRTypeArray (IRTypeStruct "Complex" []))]
              (IRTypeStruct "Complex" [])
              (IRVar "c")
        analyzeAllocations (IRProgram [structDef, func] []) `shouldSatisfy` isRight
