{-# LANGUAGE OverloadedStrings #-}
module Zap.IR.StatementSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)
import qualified Data.Set as S

import Zap.IR.Core
import Zap.IR.Statement
import Zap.Util

spec :: Spec
spec = describe "Statement IR" $ do
  describe "Basic Statements" $ do
    it "creates label statements" $ do
      let stmt = IRLabel "L1"
      case stmt of
        IRLabel label -> label `shouldBe` "L1"
        _ -> expectationFailure "Expected IRLabel constructor"

    it "creates jump statements" $ do
      let stmt = IRJump "L1"
      case stmt of
        IRJump target -> target `shouldBe` "L1"
        _ -> expectationFailure "Expected IRJump constructor"

    it "creates conditional jump statements" $ do
      let cond = IRExpr
            { metadata = IRMetadata IRTypeBool (S.singleton PureEffect) Nothing
            , expr = IRBool True
            }
      let stmt = IRCondJump cond "L1" "L2"
      case stmt of
        IRCondJump c thn els -> do
          c `shouldBe` cond
          thn `shouldBe` "L1"
          els `shouldBe` "L2"
        _ -> expectationFailure "Expected IRCondJump constructor"

  describe "Basic Blocks" $ do
    it "creates basic blocks with statements" $ do
      let stmts =
            [ IRLabel "start"
            , IRCondJump
                (mkTestExpr $ IRBool True)
                "then"
                "else"
            , IRLabel "then"
            , IRJump "end"
            , IRLabel "else"
            , IRJump "end"
            , IRLabel "end"
            ]
      let block = IRBasicBlock "entry" stmts

      blockName block `shouldBe` "entry"
      blockStmts block `shouldBe` stmts

    it "validates basic block properties" $ do
      let validBlock = IRBasicBlock "test"
            [ IRLabel "L1"
            , IRJump "L2"
            ]

      let invalidBlock = IRBasicBlock "test" []

      validateBasicBlock validBlock `shouldBe` Right ()
      validateBasicBlock invalidBlock `shouldBe`
        Left (EmptyBasicBlock "test")

  describe "Statement Sequences" $ do
    it "creates sequences of basic blocks" $ do
      let blocks =
            [ IRBasicBlock "entry"
                [ IRLabel "entry"
                , IRJump "next"
                ]
            , IRBasicBlock "next"
                [ IRLabel "next"
                , IRJump "exit"
                ]
            , IRBasicBlock "exit"
                [ IRLabel "exit"
                , IRJump "exit"
                ]
            ]
      let seq = IRSeq blocks

      seqBlocks seq `shouldBe` blocks
      validateSeq seq `shouldBe` Right ()

    it "validates sequence connectivity" $ do
      let disconnectedBlocks =
            [ IRBasicBlock "b1"
                [ IRLabel "b1"
                , IRJump "b2"
                ]
            , IRBasicBlock "b3"  -- No b2 block
                [ IRLabel "b3"
                , IRJump "exit"
                ]
            ]
      let seq = IRSeq disconnectedBlocks

      validateSeq seq `shouldBe` Left (DisconnectedFlow "b2")

    it "detects unreachable blocks" $ do
      let blocks =
            [ IRBasicBlock "entry"
                [ IRLabel "entry"
                , IRJump "exit"
                ]
            , IRBasicBlock "unreachable"  -- No jumps to this block
                [ IRLabel "unreachable"
                , IRJump "exit"
                ]
            , IRBasicBlock "exit"
                [ IRLabel "exit"
                , IRJump "exit"
                ]
            ]
      let seq = IRSeq blocks

      validateSeq seq `shouldBe` Left (UnreachableBlock "unreachable")

  describe "Expression Statements" $ do
    it "creates expression statements" $ do
      let expr = mkTestExpr $ IRBool True
      let stmt = IRExprStmt expr
      case stmt of
        IRExprStmt e -> e `shouldBe` expr
        _ -> expectationFailure "Expected IRExprStmt constructor"

    it "creates assignment statements" $ do
      let rhs = mkTestExpr $ IRNum IRInt32 "42"
      let stmt = IRAssign "x" rhs
      case stmt of
        IRAssign var expr -> do
          var `shouldBe` "x"
          expr `shouldBe` rhs
        _ -> expectationFailure "Expected IRAssign constructor"

    it "validates types in basic blocks with expressions" $ do
      let block = IRBasicBlock "test"
            [ IRExprStmt $ mkTestExpr $ IRNum IRInt32 "1"
            , IRAssign "x" $ mkTestExpr $ IRNum IRInt32 "42"
            , IRJump "next"
            ]
      validateBasicBlock block `shouldBe` Right ()

    it "handles expression statements in sequences" $ do
      let blocks =
            [ IRBasicBlock "entry"
                [ IRExprStmt $ mkTestExpr $ IRNum IRInt32 "1"
                , IRAssign "x" $ mkTestExpr $ IRNum IRInt32 "42"
                , IRJump "exit"
                ]
            , IRBasicBlock "exit"
                [ IRLabel "exit"
                , IRJump "exit"
                ]
            ]
      let seq = IRSeq blocks
      validateSeq seq `shouldBe` Right ()

  describe "Declarations and Returns" $ do
    it "creates function declarations" $ do
      let body =
            [ IRBasicBlock "entry"
                [ IRExprStmt $ mkTestExpr $ IRNum IRInt32 "42"
                , IRReturn $ Just $ mkTestExpr $ IRNum IRInt32 "42"
                ]
            ]
      let decl = IRFunction
            { funcName = "test"
            , funcParams = [("x", IRTypeNum IRInt32)]
            , funcRetType = IRTypeNum IRInt32
            , funcBody = IRSeq body
            }

      validateFunction decl `shouldBe` Right ()

    it "validates return types" $ do
      let body =
            [ IRBasicBlock "entry"
                [ IRExprStmt $ mkTestExpr $ IRNum IRInt32 "42"
                , IRReturn $ Just $ mkTestExpr $ IRString "wrong type"
                ]
            ]
      let decl = IRFunction
            { funcName = "test"
            , funcParams = []
            , funcRetType = IRTypeNum IRInt32
            , funcBody = IRSeq body
            }

      validateFunction decl `shouldBe` Left
        (TypeMismatch (IRTypeNum IRInt32) IRTypeString)

    it "requires return statement for non-void functions" $ do
      let body =
            [ IRBasicBlock "entry"
                [ IRExprStmt $ mkTestExpr $ IRNum IRInt32 "42"
                , IRJump "entry"  -- No return
                ]
            ]
      let decl = IRFunction
            { funcName = "test"
            , funcParams = []
            , funcRetType = IRTypeNum IRInt32
            , funcBody = IRSeq body
            }

      validateFunction decl `shouldBe` Left MissingReturn

    it "allows missing return for void functions" $ do
      let body =
            [ IRBasicBlock "entry"
                [ IRExprStmt $ mkTestExpr $ IRNum IRInt32 "42"
                , IRJump "entry"
                ]
            ]
      let decl = IRFunction
            { funcName = "test"
            , funcParams = []
            , funcRetType = IRTypeVoid
            , funcBody = IRSeq body
            }

      validateFunction decl `shouldBe` Right ()

  describe "Variable Declarations" $ do
    it "creates variable declarations with types" $ do
      let init = mkTestExpr $ IRNum IRInt32 "42"
      let decl = IRVarDecl "x" (IRTypeNum IRInt32) (Just init)

      case decl of
        IRVarDecl name typ mInit -> do
          name `shouldBe` "x"
          typ `shouldBe` IRTypeNum IRInt32
          mInit `shouldBe` Just init

    it "validates variable declaration types" $ do
      let wrongInit = mkTestExpr $ IRString "wrong type"
      let block = IRBasicBlock "test"
            [ IRVarDecl "x" (IRTypeNum IRInt32) (Just wrongInit)
            , IRJump "next"
            ]

      validateBasicBlock block `shouldBe`
        Left (TypeMismatch (IRTypeNum IRInt32) IRTypeString)

    it "validates variable references" $ do
      let blocks =
            [ IRBasicBlock "entry"
                [ IRVarDecl "x" (IRTypeNum IRInt32) (Just $ mkTestExpr $ IRNum IRInt32 "1")
                , IRAssign "x" (mkTestExpr $ IRNum IRInt32 "2")  -- Valid: x is declared
                , IRAssign "y" (mkTestExpr $ IRNum IRInt32 "3")  -- Invalid: y not declared
                , IRJump "exit"
                ]
            , IRBasicBlock "exit"
                [ IRReturn Nothing
                ]
            ]
      let func = IRFunction "test" [] IRTypeVoid (IRSeq blocks)

      validateFunction func `shouldBe` Left (UndeclaredVariable "y")

  describe "High-Level Control Flow" $ do
    it "validates while statement structure" $ do
      let stmt = IRWhile
            (mkTestExpr $ IRBinOp IRLt
              (mkTypedTestExpr (IRTypeNum IRInt32) $ IRVar "i")  -- Specify type
              (mkTestExpr $ IRNum IRInt32 "10"))
            [ IRVarDecl "i" (IRTypeNum IRInt32) Nothing,
              IRAssign "i" (mkTypedTestExpr (IRTypeNum IRInt32) $ IRVar "i")
            ]
      validateBasicBlock (IRBasicBlock "test" [IRVarDecl "i" (IRTypeNum IRInt32) Nothing, stmt])
        `shouldBe` Right ()

    it "allows statements after while loops in blocks" $ do
      let stmts =
            [ IRVarDecl "i" (IRTypeNum IRInt32) Nothing,  -- Declare i first
              IRWhile
                (mkTestExpr $ IRBinOp IRLt
                  (mkTypedTestExpr (IRTypeNum IRInt32) $ IRVar "i")  -- Specify type
                  (mkTestExpr $ IRNum IRInt32 "10"))
                [ IRAssign "i" (mkTestExpr $ IRBinOp IRAdd
                    (mkTypedTestExpr (IRTypeNum IRInt32) $ IRVar "i")  -- Specify type
                    (mkTestExpr $ IRNum IRInt32 "1"))
                ],
              IRReturn Nothing
            ]
      validateBasicBlock (IRBasicBlock "test" stmts) `shouldBe` Right ()

  describe "While Loop Validation" $ do
    it "validates condition type is boolean" $ do
      let stmt = IRWhile
            (mkTestExpr $ IRNum IRInt32 "42")  -- Non-boolean condition
            [ IRAssign "x" (mkTestExpr $ IRNum IRInt32 "1") ]
      validateBasicBlock (IRBasicBlock "test" [stmt]) `shouldBe`
        Left (TypeMismatch IRTypeBool (IRTypeNum IRInt32))

    it "validates variables used in loop are in scope" $ do
      let stmt = IRWhile
            (mkTestExpr $ IRBinOp IRLt
              (mkTypedTestExpr (IRTypeNum IRInt32) $ IRVar "x")  -- x not declared
              (mkTestExpr $ IRNum IRInt32 "10"))
            []
      validateBasicBlock (IRBasicBlock "test" [stmt]) `shouldBe`
        Left (UndeclaredVariable "x")

    it "maintains scope for variables declared in loop body" $ do
      let stmt = IRWhile
            (mkTestExpr $ IRBool True)
            [ IRVarDecl "i" (IRTypeNum IRInt32)
                (Just (mkTestExpr $ IRNum IRInt32 "0"))
            , IRAssign "i" (mkTestExpr $ IRNum IRInt32 "1")
            ]
      validateBasicBlock (IRBasicBlock "test" [stmt]) `shouldBe` Right ()

  describe "Loop Variable Analysis" $ do
    it "validates modification of loop counter" $ do
      let stmt = IRWhile
            (mkTestExpr $ IRBinOp IRLt
              (mkTypedTestExpr (IRTypeNum IRInt32) $ IRVar "i")
              (mkTestExpr $ IRNum IRInt32 "10"))
            [ IRVarDecl "i" (IRTypeNum IRInt32) Nothing
            ] -- Missing increment of i
      validateBasicBlock (IRBasicBlock "test" [IRVarDecl "i" (IRTypeNum IRInt32) Nothing, stmt])
        `shouldBe` Left (UnmodifiedLoopVar "i")

    it "accepts valid loop counter modification" $ do
      let stmt = IRWhile
            (mkTestExpr $ IRBinOp IRLt
              (mkTypedTestExpr (IRTypeNum IRInt32) $ IRVar "i")
              (mkTestExpr $ IRNum IRInt32 "10"))
            [ IRVarDecl "i" (IRTypeNum IRInt32) Nothing,
              IRAssign "i" (mkTestExpr $ IRBinOp IRAdd
                (mkTypedTestExpr (IRTypeNum IRInt32) $ IRVar "i")
                (mkTestExpr $ IRNum IRInt32 "1"))
            ]
      validateBasicBlock (IRBasicBlock "test" [IRVarDecl "i" (IRTypeNum IRInt32) Nothing, stmt])
        `shouldBe` Right ()
