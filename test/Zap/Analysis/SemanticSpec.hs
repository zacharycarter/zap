{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.SemanticSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import qualified Data.Map.Strict as M

import Zap.AST
import Zap.Analysis.Semantic

spec :: Spec
spec = do
  describe "Function Analysis" $ do
    describe "Function Definitions" $ do
      it "accepts valid function definitions" $ do
        let ast = Program
              [ TLDecl $ DFunc "add"
                  [ Param "x" (TypeNum Int32)
                  , Param "y" (TypeNum Int32)
                  ]
                  (TypeNum Int32)
                  (BinOp Add (Var "x") (Var "y"))
              ]
        analyze ast `shouldBe` Right ast

      it "detects type mismatches in function bodies" $ do
        let ast = Program
              [ TLDecl $ DFunc "broken"
                  [ Param "x" (TypeNum Int32) ]
                  (TypeNum Int32)
                  (StrLit "wrong type")
              ]
        analyze ast `shouldBe` Left (TypeMismatchInFunction "broken" (TypeNum Int32) TypeString)

      it "prevents duplicate function definitions" $ do
        let ast = Program
              [ TLDecl $ DFunc "f" [] TypeBool (BoolLit True)
              , TLDecl $ DFunc "f" [] TypeBool (BoolLit False)
              ]
        analyze ast `shouldBe` Left (RecursionInGlobalScope "f")

    describe "Function Calls" $ do
      it "validates function call arguments" $ do
        let ast = Program
              [ TLDecl $ DFunc "add"
                  [ Param "x" (TypeNum Int32)
                  , Param "y" (TypeNum Int32)
                  ]
                  (TypeNum Int32)
                  (BinOp Add (Var "x") (Var "y"))
              , TLExpr $ Call "add"
                  [ NumLit Int32 "1"
                  , NumLit Int32 "2"
                  ]
              ]
        analyze ast `shouldBe` Right ast

      it "detects calls to undefined functions" $ do
        let ast = Program
              [ TLExpr $ Call "undefined" [] ]
        analyze ast `shouldBe` Left (UndefinedFunction "undefined")

      it "checks argument count" $ do
        let ast = Program
              [ TLDecl $ DFunc "add"
                  [ Param "x" (TypeNum Int32)
                  , Param "y" (TypeNum Int32)
                  ]
                  (TypeNum Int32)
                  (BinOp Add (Var "x") (Var "y"))
              , TLExpr $ Call "add" [ NumLit Int32 "1" ]
              ]
        analyze ast `shouldBe` Left (ArgumentCountMismatch "add" 2 1)

      it "checks argument types" $ do
        let ast = Program
              [ TLDecl $ DFunc "add"
                  [ Param "x" (TypeNum Int32)
                  , Param "y" (TypeNum Int32)
                  ]
                  (TypeNum Int32)
                  (BinOp Add (Var "x") (Var "y"))
              , TLExpr $ Call "add"
                  [ NumLit Int32 "1"
                  , StrLit "not a number"
                  ]
              ]
        analyze ast `shouldBe` Left (TypeMismatchInFunction "add" (TypeNum Int32) TypeString)

    describe "Scoping Rules" $ do
      it "allows access to parameters within function bodies" $ do
        let ast = Program
              [ TLDecl $ DFunc "identity"
                  [ Param "x" (TypeNum Int32) ]
                  (TypeNum Int32)
                  (Var "x")
              ]
        analyze ast `shouldBe` Right ast

      it "prevents access to parameters outside function bodies" $ do
        let ast = Program
              [ TLDecl $ DFunc "f"
                  [ Param "x" (TypeNum Int32) ]
                  (TypeNum Int32)
                  (Var "x")
              , TLExpr $ Var "x"
              ]
        analyze ast `shouldBe` Left (UndefinedVariable "x")

    describe "Basic Expression Analysis" $ do
      it "analyzes string literals" $ do
        let ast = Program [TLExpr $ StrLit "hello"]
        analyze ast `shouldBe` Right ast

      it "rejects empty string literals" $ do
        let ast = Program [TLExpr $ StrLit ""]
        analyze ast `shouldBe` Left EmptyStringLiteral

      it "analyzes numeric literals" $ do
        let ast = Program [TLExpr $ NumLit Int32 "42"]
        analyze ast `shouldBe` Right ast

    describe "Advanced Function Analysis" $ do
      it "validates chained function calls" $ do
        let ast = Program
              [ TLDecl $ DFunc "double"
                  [ Param "x" (TypeNum Int32) ]
                  (TypeNum Int32)
                  (BinOp Add (Var "x") (Var "x"))
              , TLDecl $ DFunc "quadruple"
                  [ Param "x" (TypeNum Int32) ]
                  (TypeNum Int32)
                  (Call "double" [Call "double" [Var "x"]])
              ]
        analyze ast `shouldBe` Right ast

      it "validates functions with vector return types" $ do
        let ast = Program
              [ TLDecl $ DFunc "makeVec2"
                  [ Param "x" (TypeNum Float32)
                  , Param "y" (TypeNum Float32)
                  ]
                  (TypeVec (Vec2 Float32))
                  (VecLit (Vec2 Float32)
                    [ Var "x"
                    , Var "y"
                    ])
              ]
        analyze ast `shouldBe` Right ast

      it "handles shadowing in nested scopes" $ do
        let ast = Program
              [ TLDecl $ DFunc "outer"
                  [ Param "x" (TypeNum Int32) ]
                  (TypeNum Int32)
                  (Let "y"
                    (BinOp Add
                      (Var "x")
                      (Let "y" (NumLit Int32 "10"))))
              ]
        analyze ast `shouldBe` Right ast

      it "validates function calls within vector literals" $ do
        let ast = Program
              [ TLDecl $ DFunc "getX"
                  [ Param "x" (TypeNum Float32) ]
                  (TypeNum Float32)
                  (Var "x")
              , TLDecl $ DFunc "makeVec"
                  [ Param "x" (TypeNum Float32) ]
                  (TypeVec (Vec2 Float32))
                  (VecLit (Vec2 Float32)
                    [ Call "getX" [Var "x"]
                    , NumLit Float32 "0.0"
                    ])
              ]
        analyze ast `shouldBe` Right ast

    describe "Block Type Analysis" $ do
      it "infers type from last expression in block" $ do
        let ast = Program
              [ TLExpr $ Block $ BlockScope
                  { blockLabel = "test"
                  , blockExprs = [Call "print" [StrLit "Hello"]]
                  , blockResult = Just (NumLit Int32 "42")
                  }
              ]
        case analyze ast of
          Right _ -> return ()
          Left err -> expectationFailure $ "Expected success but got: " ++ show err

      it "validates block result type matches enclosing function" $ do
        let ast = Program
              [ TLDecl $ DFunc "blockFunc"
                  [ Param "x" (TypeNum Int32) ]
                  (TypeNum Int32)
                  (Block $ BlockScope
                    { blockLabel = "inner"
                    , blockExprs = [Call "print" [StrLit "Hello"]]
                    , blockResult = Just (Var "x")
                    })
              ]
        analyze ast `shouldBe` Right ast

      it "rejects blocks with type mismatches" $ do
        let ast = Program
              [ TLDecl $ DFunc "badBlock"
                  [ Param "x" (TypeNum Int32) ]
                  (TypeNum Int32)
                  (Block $ BlockScope
                    { blockLabel = "inner"
                    , blockExprs = [Call "print" [StrLit "Hello"]]
                    , blockResult = Just (StrLit "wrong type")
                    })
              ]
        case analyze ast of
          Left (TypeMismatchInFunction _ _ _) -> return ()
          other -> expectationFailure $
            "Expected type mismatch error but got: " ++ show other

      it "maintains correct variable scope in nested blocks" $ do
        let ast = Program
              [ TLDecl $ DFunc "nestedBlocks"
                  [ Param "x" (TypeNum Int32) ]
                  (TypeNum Int32)
                  (Block $ BlockScope
                    { blockLabel = "outer"
                    , blockExprs =
                        [ Let "y" (NumLit Int32 "5")
                        , Block $ BlockScope
                            { blockLabel = "inner"
                            , blockExprs = []
                            , blockResult = Just (BinOp Add (Var "x") (Var "y"))
                            }
                        ]
                    , blockResult = Just (Var "x")
                    })
              ]
        analyze ast `shouldBe` Right ast

    describe "Vector Type Analysis" $ do
      it "validates vector arithmetic operations" $ do
        let ast = Program
              [ TLDecl $ DFunc "vecAdd"
                  [ Param "v1" (TypeVec (Vec2 Float32))
                  , Param "v2" (TypeVec (Vec2 Float32))
                  ]
                  (TypeVec (Vec2 Float32))
                  (BinOp Add (Var "v1") (Var "v2"))
              ]
        analyze ast `shouldBe` Right ast

      it "validates vector dot product" $ do
        let ast = Program
              [ TLDecl $ DFunc "dotProduct"
                  [ Param "v1" (TypeVec (Vec4 Float32))
                  , Param "v2" (TypeVec (Vec4 Float32))
                  ]
                  (TypeNum Float32)
                  (BinOp Dot (Var "v1") (Var "v2"))
              ]
        analyze ast `shouldBe` Right ast

      it "validates vector literal components" $ do
        let ast = Program
              [ TLExpr $ VecLit (Vec2 Float32)
                  [ NumLit Float32 "1.0"
                  , NumLit Float32 "2.0"
                  ]
              ]
        analyze ast `shouldBe` Right ast

      it "rejects vector literals with mismatched component types" $ do
        let ast = Program
              [ TLExpr $ VecLit (Vec2 Float32)
                  [ NumLit Float32 "1.0"
                  , NumLit Int32 "2"
                  ]
              ]
        analyze ast `shouldBe`
          Left (InvalidVectorComponents
            (Vec2 Float32)
            [TypeNum Float32, TypeNum Int32])

      it "rejects vector literals with wrong number of components" $ do
        let ast = Program
              [ TLExpr $ VecLit (Vec3 Float32)
                  [ NumLit Float32 "1.0"
                  , NumLit Float32 "2.0"
                  ]
              ]
        analyze ast `shouldBe`
          Left (InvalidVectorComponents
            (Vec3 Float32)
            [TypeNum Float32, TypeNum Float32])

      it "validates component access across different vector sizes" $ do
        let makeAst field vecType = Program
              [ TLExpr $ FieldAccess
                  (VecLit vecType
                    (replicate (case vecType of
                      Vec2 _ -> 2
                      Vec3 _ -> 3
                      Vec4 _ -> 4) (NumLit Float32 "1.0")))
                  field
              ]

        let testCases =
              [ (Vec2 Float32, "x", Right $ makeAst "x" (Vec2 Float32))
              , (Vec2 Float32, "y", Right $ makeAst "y" (Vec2 Float32))
              , (Vec2 Float32, "z", Left $ UndefinedField "Vec2 Float32" "z")
              , (Vec3 Float32, "z", Right $ makeAst "z" (Vec3 Float32))
              , (Vec4 Float32, "w", Right $ makeAst "w" (Vec4 Float32))
              , (Vec4 Float32, "q", Left $ UndefinedField "Vec4 Float32" "q")
              ]

        forM_ testCases $ \(vecType, field, expected) ->
          analyze (makeAst field vecType) `shouldBe` expected

      it "rejects operations between incompatible vector types" $ do
        let ast = Program
              [ TLDecl $ DFunc "invalidAdd"
                  [ Param "v2" (TypeVec (Vec2 Float32))
                  , Param "v3" (TypeVec (Vec3 Float32))
                  ]
                  (TypeVec (Vec2 Float32))
                  (BinOp Add (Var "v2") (Var "v3"))
              ]
        analyze ast `shouldBe`
          Left (TypeMismatchInOp Add
            (TypeVec (Vec2 Float32))
            (TypeVec (Vec3 Float32)))

  describe "Type Inference" $ do
    describe "Let Binding Type Inference" $ do
      it "infers types from numeric initialization" $ do
        let ast = Program [TLExpr (Let "x" (NumLit Int32 "42"))]
        case analyze ast of
          Right (Program [TLExpr (Let "x" _)]) -> do
            case getVarTypes ast of
              Right types -> M.lookup "x" types `shouldBe` Just (TypeNum Int32)
              Left err -> expectationFailure $ "Failed to get variable types: " ++ show err
          Right other -> expectationFailure $
            "Unexpected AST structure: " ++ show other
          Left err -> expectationFailure $
            "Analysis failed: " ++ show err

      it "infers numeric types through binary operations" $ do
        let ast = Program
              [ TLExpr $ Let "x" (NumLit Int32 "1")
              , TLExpr $ Let "y" (BinOp Add (Var "x") (NumLit Int32 "2"))
              ]
        case analyze ast of
          Right _ -> case getVarTypes ast of
            Right types -> M.lookup "y" types `shouldBe` Just (TypeNum Int32)
            Left err -> expectationFailure $ "Failed to get variable types: " ++ show err
          Left err -> expectationFailure $
            "Analysis failed: " ++ show err

    describe "Function Type Inference" $ do
      it "infers parameter and return types from numeric operations" $ do
        let ast = Program
              [ TLDecl $ DFunc "add"
                  [Param "x" TypeAny, Param "y" TypeAny]  -- Unspecified types
                  TypeAny  -- Unspecified return type
                  (BinOp Add (Var "x") (Var "y"))
              ]
        case analyze ast of
          Right _ -> do
            case getVarTypes ast of
              Right types -> do
                M.lookup "x" types `shouldBe` Just (TypeNum Int32)
                M.lookup "y" types `shouldBe` Just (TypeNum Int32)
              Left err -> expectationFailure $ "Failed to get variable types: " ++ show err
          Left err -> expectationFailure $ "Analysis failed: " ++ show err

    it "handles mixed TypeAny and concrete type parameters" $ do
      let ast = Program
            [ TLDecl $ DFunc "mixedTypes"
                [Param "x" TypeAny, Param "y" (TypeNum Int32)]
                (TypeNum Int32)  -- Concrete return type
                (BinOp Add (Var "x") (Var "y"))
            ]
      analyze ast `shouldBe` Right ast

    it "infers parameter types without breaking scoping" $ do
      let ast = Program
            [ TLDecl $ DFunc "add"
                [Param "x" TypeAny, Param "y" TypeAny]
                TypeAny
                (BinOp Add (Var "x") (Var "y"))
            , TLExpr $ Var "x"  -- Should fail
            ]
      analyze ast `shouldBe` Left (UndefinedVariable "x")
      case getVarTypes ast of
        Right types -> M.lookup "x" types `shouldBe` Just (TypeNum Int32)
        Left err -> expectationFailure $ "Analysis failed: " ++ show err
