{-# LANGUAGE OverloadedStrings #-}
module Zap.IRSpec (spec) where

import Test.Hspec
import qualified Data.Set as S

import Zap.AST
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

    describe "Type Inference" $ do
      it "generates constraints for function definitions" $ do
        let func = IRFuncDecl
              { fnName = "test"
              , fnParams = [("x", IRTypeInt)]
              , fnRetType = IRTypeInt
              , fnBody = IRBlock "entry"
                  [(IRReturn (Just (IRCall "+" [IRLit (IRIntLit 1), IRLit (IRIntLit 2)])), testMeta)]
              }
        let prog = IRProgram [(func, testMeta)]

        case generateConstraints prog of
          Right constraints -> do
            -- Should include:
            -- 1. Parameter type constraint
            -- 2. Return type constraint
            -- 3. Two function call argument constraints (one per int literal)
            length constraints `shouldBe` 4
            constraints `shouldContain` [TEq IRTypeInt IRTypeInt]  -- Parameter constraint
            -- List all expected constraints
            let expectedConstraints = [
                  TEq IRTypeInt IRTypeInt,  -- Parameter
                  TEq IRTypeInt IRTypeInt,  -- Return
                  TEq IRTypeInt IRTypeInt,  -- First arg
                  TEq IRTypeInt IRTypeInt   -- Second arg
                  ]
            constraints `shouldMatchList` expectedConstraints
          Left err -> expectationFailure $ show err

      it "handles return type constraints" $ do
        let func = IRFuncDecl
              { fnName = "test"
              , fnParams = []
              , fnRetType = IRTypeInt
              , fnBody = IRBlock "entry"
                  [(IRReturn (Just (IRCall "id" [IRLit (IRIntLit 42)])), testMeta)]
              }
        let prog = IRProgram [(func, testMeta)]

        case generateConstraints prog of
          Right constraints -> do
            -- Should ensure return type matches function signature
            constraints `shouldContain` [TEq IRTypeInt IRTypeInt]
          Left err -> expectationFailure $ show err

    describe "Print statement conversion" $ do
      it "converts string literal print to procedure call" $ do
          let ast = Program [TLExpr (Call "print" [StrLit "test"])]
          case convertToIR' ast of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case head stmts of
                      (IRProcCall "print" [IRLit (IRStringLit "test")], _) -> return ()
                      other -> expectationFailure $ "Expected procedure call, got: " ++ show other

      it "converts print with binary operation to procedure call" $ do
          let ast = Program [TLExpr (Call "print" [BinOp Add (NumLit Int32 "1") (NumLit Int32 "2")])]
          case convertToIR' ast of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case head stmts of
                      (IRProcCall "print" [IRLit (IRIntLit 3)], _) -> return ()
                      other -> expectationFailure $ "Expected procedure call, got: " ++ show other
    it "converts print to procedure call" $ do
      let ast = Program [TLExpr (Call "print" [StrLit "test"])]
      case convertToIR' ast of
        Right (IRProgram [(mainFn, _)]) -> do
          let IRBlock _ stmts = fnBody mainFn
          case head stmts of
            (IRProcCall "print" [IRLit (IRStringLit "test")], _) -> return ()
            _ -> expectationFailure "Expected procedure call"

    describe "Print statement conversion" $ do
      it "converts string literal print to procedure call" $ do
          let ast = Program [TLExpr (Call "print" [StrLit "test"])]
          case convertToIR' ast of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case head stmts of
                      (IRProcCall "print" [IRLit (IRStringLit "test")], _) -> return ()
                      other -> expectationFailure $ "Expected procedure call, got: " ++ show other

      it "converts print with binary operation to procedure call" $ do
          let ast = Program [TLExpr (Call "print" [BinOp Add (NumLit Int32 "1") (NumLit Int32 "2")])]
          case convertToIR' ast of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case head stmts of
                      (IRProcCall "print" [IRLit (IRIntLit 3)], _) -> return ()
                      other -> expectationFailure $ "Expected procedure call, got: " ++ show other
      where
        testMeta = IRMetadata IRTypeInt (S.singleton PureEffect) Nothing

