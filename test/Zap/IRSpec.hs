{-# LANGUAGE OverloadedStrings #-}
module Zap.IRSpec (spec) where

import Test.Hspec
import qualified Data.Set as S

import Zap.AST (Program(..), TopLevel(..), Expr(..))
import Zap.IR

spec :: Spec
spec = do
  describe "IR Conversion" $ do
    describe "Basic Program Structure" $ do
      it "creates a main function for top-level expressions" $ do
        let ast = Program [TLExpr (Call "print" [StrLit "test"])]
        case convertToIR' ast of
          Right (IRProgram funcs) -> do
            length funcs `shouldBe` 1
            case funcs of
              [(mainFn, meta)] -> do
                fnName mainFn `shouldBe` "main"
                fnParams mainFn `shouldBe` []
                fnRetType mainFn `shouldBe` IRTypeVoid
                metaType meta `shouldBe` IRTypeVoid
                metaEffects meta `shouldBe` S.singleton PureEffect
              _ -> expectationFailure "Expected single main function"
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

      it "adds implicit return to main block" $ do
        let ast = Program [TLExpr (Call "print" [StrLit "test"])]
        case convertToIR' ast of
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case last stmts of
              (IRReturn Nothing, meta) -> do
                metaType meta `shouldBe` IRTypeVoid
                metaEffects meta `shouldBe` S.singleton PureEffect
              _ -> expectationFailure "Expected implicit return"
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

    describe "Print Statement Conversion" $ do
      it "converts print with string literal" $ do
        let ast = Program [TLExpr (Call "print" [StrLit "Hello, World!"])]
        case convertToIR' ast of
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock label stmts = fnBody mainFn
            label `shouldBe` "main.entry"
            length stmts `shouldBe` 2  -- print + implicit return
            case head stmts of
              (IRStmtExpr (IRCall "print" [IRStringLit "Hello, World!"]), meta) -> do
                metaType meta `shouldBe` IRTypeVoid
                metaEffects meta `shouldBe` S.singleton IOEffect
              _ -> expectationFailure "Expected print statement"
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

    describe "Error Handling" $ do
      it "reports error for unsupported expressions" $ do
        let ast = Program [TLExpr (Call "unknown" [])]
        case convertToIR' ast of
          Left (IRUnsupportedExpr _) -> return ()
          Left err -> expectationFailure $ "Expected unsupported expr error, got: " ++ show err
          Right _ -> expectationFailure "Expected error for unsupported expression"

      it "reports error for print with no arguments" $ do
        let ast = Program [TLExpr (Call "print" [])]
        case convertToIR' ast of
          Left (IRInvalidFunction _) -> return ()
          Left err -> expectationFailure $ "Expected invalid function error, got: " ++ show err
          Right _ -> expectationFailure "Expected error for invalid print call"

    describe "IR Metadata" $ do
      it "tracks IOEffect for print statements" $ do
        let ast = Program [TLExpr (Call "print" [StrLit "test"])]
        case convertToIR' ast of
          Right (IRProgram [(mainFn, fnMeta)]) -> do
            -- Test function metadata
            metaEffects fnMeta `shouldBe` S.singleton PureEffect

            -- Test statement metadata directly from the function body
            let IRBlock _ stmts = fnBody mainFn
            case head stmts of
              (_, stmtMeta) -> metaEffects stmtMeta `shouldBe` S.singleton IOEffect

          Left err -> expectationFailure $ "Conversion failed: " ++ show err
