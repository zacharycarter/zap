{-# LANGUAGE OverloadedStrings #-}
module Zap.IRSpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

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
                metaEffects meta `shouldBe` S.singleton IOEffect
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
            metaEffects fnMeta `shouldBe` S.singleton IOEffect

            -- Test statement metadata directly from the function body
            let IRBlock _ stmts = fnBody mainFn
            case head stmts of
              (_, stmtMeta) -> metaEffects stmtMeta `shouldBe` S.singleton IOEffect

          Left err -> expectationFailure $ "Conversion failed: " ++ show err

    describe "Type Inference" $ do
      it "generates constraints for function definitions" $ do
        let func = IRFuncDecl
              { fnName = "test"
              , fnParams = [("x", IRTypeInt32)]
              , fnRetType = IRTypeInt32
              , fnBody = IRBlock "entry"
                  [(IRReturn (Just (IRCall "+" [IRLit (IRInt32Lit 1), IRLit (IRInt32Lit 2)])), testMeta)]
              }
        let prog = IRProgram [(func, testMeta)]

        case generateConstraints prog of
          Right constraints -> do
            -- Should include:
            -- 1. Parameter type constraint
            -- 2. Return type constraint
            -- 3. Two function call argument constraints (one per int literal)
            length constraints `shouldBe` 4
            constraints `shouldContain` [TEq IRTypeInt32 IRTypeInt32]  -- Parameter constraint
            -- List all expected constraints
            let expectedConstraints = [
                  TEq IRTypeInt32 IRTypeInt32,  -- Parameter
                  TEq IRTypeInt32 IRTypeInt32,  -- Return
                  TEq IRTypeInt32 IRTypeInt32,  -- First arg
                  TEq IRTypeInt32 IRTypeInt32   -- Second arg
                  ]
            constraints `shouldMatchList` expectedConstraints
          Left err -> expectationFailure $ show err

      it "handles return type constraints" $ do
        let func = IRFuncDecl
              { fnName = "test"
              , fnParams = []
              , fnRetType = IRTypeInt32
              , fnBody = IRBlock "entry"
                  [(IRReturn (Just (IRCall "id" [IRLit (IRInt32Lit 42)])), testMeta)]
              }
        let prog = IRProgram [(func, testMeta)]

        case generateConstraints prog of
          Right constraints -> do
            -- Should ensure return type matches function signature
            constraints `shouldContain` [TEq IRTypeInt32 IRTypeInt32]
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
                      (IRProcCall "print" [IRLit (IRInt32Lit 3)], _) -> return ()
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
                      (IRProcCall "print" [IRLit (IRInt32Lit 3)], _) -> return ()
                      other -> expectationFailure $ "Expected procedure call, got: " ++ show other


    describe "Type Checking" $ do
      it "detects type errors in function calls" $ do
        let func = IRFuncDecl
              { fnName = "add"
              , fnParams = [("x", IRTypeInt32), ("y", IRTypeInt32)]
              , fnRetType = IRTypeInt32
              , fnBody = IRBlock "entry"
                  [(IRReturn (Just (IRCall "add"
                    [IRLit (IRStringLit "not_a_number"),
                     IRLit (IRInt32Lit 42)])), testMeta)]
              }
        let prog = IRProgram [(func, testMeta)]
        let constraints = generateConstraints prog
        case constraints of
          Right cs -> do
            solveConstraints cs `shouldBe`
              Left (UnificationError IRTypeString IRTypeInt32)
          Left err -> expectationFailure $ "Constraint generation failed: " ++ show err

      it "detects type errors in binary operations" $ do
          let expr = IRCall "Add"
                [ IRLit (IRStringLit "hello")
                , IRLit (IRInt32Lit 42) ]
          let prog = IRProgram [(IRFuncDecl
                                 { fnName = "main"
                                 , fnParams = []
                                 , fnRetType = IRTypeVoid
                                 , fnBody = IRBlock "entry" [(IRStmtExpr expr, testMeta)]
                                 }, testMeta)]

          let constraints = generateConstraints prog
          case constraints of
            Right cs -> do
              traceM $ "Generated constraints: " ++ show cs
              solveConstraints cs `shouldBe`
                Left (UnificationError IRTypeString IRTypeInt32)
            Left err -> expectationFailure $
              "Constraint generation failed: " ++ show err


      it "handles 32-bit floating point arithmetic" $ do
        let expr = IRCall "Add"
              [ IRLit (IRFloat32Lit 3.14)
              , IRLit (IRFloat32Lit 2.86) ]
        let prog = IRProgram [(IRFuncDecl
                               { fnName = "main"
                               , fnParams = []
                               , fnRetType = IRTypeFloat32
                               , fnBody = IRBlock "entry" [(IRStmtExpr expr, testMeta)]
                               }, testMeta)]

        let constraints = generateConstraints prog
        case constraints of
          Right cs -> do
            traceM $ "Generated constraints: " ++ show cs
            case solveConstraints cs of
              Right subst ->
                M.map (const IRTypeFloat32) subst `shouldBe` subst
              Left err -> expectationFailure $
                "Constraint solving failed: " ++ show err
          Left err -> expectationFailure $
            "Constraint generation failed: " ++ show err

      it "handles mixed numeric type operations correctly" $ do
        let expr = IRCall "Add"
              [ IRLit (IRFloat32Lit 1.0)
              , IRCall "Mul"
                  [ IRLit (IRInt32Lit 2)
                  , IRLit (IRFloat32Lit 3.0)
                  ]
              ]
        let prog = IRProgram [(IRFuncDecl
                               { fnName = "main"
                               , fnParams = []
                               , fnRetType = IRTypeFloat32
                               , fnBody = IRBlock "entry" [(IRStmtExpr expr, testMeta)]
                               }, testMeta)]

        traceM "\n=== Type Inference Test ==="
        case generateConstraints prog of
            Right constraints -> do
                traceM $ "Generated constraints: " ++ show constraints
                case solveConstraints constraints of
                    Right subst -> do
                        -- Check the final type after substitution
                        let resultType = applySubst subst (metaType testMeta)
                        traceM $ "Result type: " ++ show resultType
                        resultType `shouldBe` IRTypeFloat32
                    Left err -> expectationFailure $ "Constraint solving failed: " ++ show err
            Left err -> expectationFailure $ "Constraint generation failed: " ++ show err

    describe "Type coercion rules" $ do
      it "follows strict numeric type hierarchy" $ do
          let irInt32 = IRLit (IRInt32Lit 1)
          let irInt64 = IRLit (IRInt64Lit 1)
          let irFloat32 = IRLit (IRFloat32Lit 1.0)
          let irFloat64 = IRLit (IRFloat64Lit 1.0)

          let prog = IRProgram [(IRFuncDecl
                                 { fnName = "test"
                                 , fnParams = []
                                 , fnRetType = IRTypeFloat64
                                 , fnBody = IRBlock "entry"
                                     [(IRStmtExpr $ IRCall "Add" [irInt32, irFloat64], testMeta)]
                                 }, testMeta)]

          case generateConstraints prog of
              Right constraints -> do
                  traceM $ "Generated constraints: " ++ show constraints
                  case solveConstraints constraints of
                      Right subst -> do
                          let resultType = applySubst subst (metaType testMeta)
                          resultType `shouldBe` IRTypeFloat64  -- Higher precision wins

      it "preserves type precision in same-type operations" $ do
        -- Test Int32 precision preservation
        let int32Expr = IRCall "Add"
              [ IRLit (IRInt32Lit 1)
              , IRLit (IRInt32Lit 2)
              ]
        let int32Prog = IRProgram [(IRFuncDecl
                                    { fnName = "test"
                                    , fnParams = []
                                    , fnRetType = IRTypeInt32
                                    , fnBody = IRBlock "entry" [(IRStmtExpr int32Expr, testMeta)]
                                    }, testMeta)]

        -- Test Float32 precision preservation
        let float32Expr = IRCall "Add"
              [ IRLit (IRFloat32Lit 1.0)
              , IRLit (IRFloat32Lit 2.0)
              ]
        let float32Prog = IRProgram [(IRFuncDecl
                                      { fnName = "test"
                                      , fnParams = []
                                      , fnRetType = IRTypeFloat32
                                      , fnBody = IRBlock "entry" [(IRStmtExpr float32Expr, testMeta)]
                                      }, testMeta)]

        -- Verify Int32 + Int32 -> Int32
        case generateConstraints int32Prog of
            Right constraints -> do
                case solveConstraints constraints of
                    Right subst -> do
                        let resultType = applySubst subst (metaType testMeta)
                        resultType `shouldBe` IRTypeInt32
                    Left err -> expectationFailure $ "Int32 constraint solving failed: " ++ show err
            Left err -> expectationFailure $ "Int32 constraint generation failed: " ++ show err

        -- Verify Float32 + Float32 -> Float32
        case generateConstraints float32Prog of
            Right constraints -> do
                case solveConstraints constraints of
                    Right subst -> do
                        let resultType = applySubst subst (metaType testMeta)
                        resultType `shouldBe` IRTypeFloat32
                    Left err -> expectationFailure $ "Float32 constraint solving failed: " ++ show err
            Left err -> expectationFailure $ "Float32 constraint generation failed: " ++ show err

    describe "Struct type handling" $ do
      it "converts struct definitions to IR" $ do
        let ast = Program
              [ TLType "Point" (TypeStruct "Point" [("x", TypeNum Float32), ("y", TypeNum Float32)])
              , TLExpr (Let "p" (StructLit "Point" [("x", NumLit Float32 "1.0"), ("y", NumLit Float32 "2.0")]))
              , TLExpr (Call "print" [FieldAccess (Var "p") "x"])
              ]

        case convertToIR' ast of
          Right (IRProgram funcs) -> do
            length funcs `shouldBe` 1  -- Should be main function
            case funcs of
              [(mainFn, _)] -> do
                let IRBlock _ stmts = fnBody mainFn
                case stmts of
                  [(IRVarDecl "p" typ _, _),
                   (IRProcCall "print" [IRCall "field_access" [IRVar "p", IRLit (IRStringLit "x")]], meta),
                   (IRReturn Nothing, _)] -> do  -- Add expectation for implicit return
                    typ `shouldBe` IRTypeStruct "Point" [("x", IRTypeFloat32), ("y", IRTypeFloat32)]
                    metaType meta `shouldBe` IRTypeFloat32  -- Field access should have float type
                  _ -> expectationFailure $ "Unexpected statements: " ++ show stmts
              _ -> expectationFailure "Expected single main function"
          Left err -> expectationFailure $ "IR conversion failed: " ++ show err

      describe "Literal conversion" $ do
        it "preserves literal type information in IR" $ do
          let ast = Program [TLExpr (Let "x" (Lit (IntLit "42" (Just Int32))))]
          case convertToIR' ast of
            Right (IRProgram funcs) -> do
              case funcs of
                [(mainFn, _)] -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case head stmts of
                    (IRVarDecl "x" _ (IRLit lit), meta) -> do
                      metaLiteralType meta `shouldBe` Just (LitInt Int32)
                    _ -> expectationFailure "Expected variable declaration"
            Left err -> expectationFailure $ "IR conversion failed: " ++ show err

        it "preserves float literal type information" $ do
          let ast = Program [TLExpr (Let "x" (Lit (FloatLit "3.14" (Just Float32))))]
          case convertToIR' ast of
            Right (IRProgram [(mainFn, _)]) -> do
              case fnBody mainFn of
                IRBlock _ ((IRVarDecl _ _ _, meta):_) ->
                  metaLiteralType meta `shouldBe` Just (LitFloat Float32)
                _ -> expectationFailure "Expected variable declaration"
            Left err -> expectationFailure $ show err

        it "preserves string literal type information" $ do
          let ast = Program [TLExpr (Let "x" (Lit (StringLit "test")))]
          case convertToIR' ast of
            Right (IRProgram [(mainFn, _)]) -> do
              case fnBody mainFn of
                IRBlock _ ((IRVarDecl _ _ _, meta):_) ->
                  metaLiteralType meta `shouldBe` Just LitString
                _ -> expectationFailure "Expected variable declaration"

        it "handles both NumLit and Lit variants" $ do
            let ast = Program
                  [ TLExpr (Call "print" [NumLit Int32 "42"])  -- Old style
                  , TLExpr (Call "print" [Lit (IntLit "42" (Just Int32))])  -- New style
                  ]
            case convertToIR' ast of
                Right (IRProgram [(mainFn, _)]) -> do
                    let IRBlock _ stmts = fnBody mainFn
                    length stmts `shouldBe` 3  -- Two prints plus return
                Left err -> expectationFailure $ show err

        it "preserves numeric types" $ do
            let ast = Program [TLExpr (Call "print" [Lit (FloatLit "3.14" (Just Float32))])]
            case convertToIR' ast of
                Right (IRProgram [(mainFn, _)]) -> do
                    let IRBlock _ ((IRProcCall "print" [IRLit lit], _):_) = fnBody mainFn
                    lit `shouldBe` IRFloat32Lit 3.14
                Left err -> expectationFailure $ show err

      describe "Variable declarations" $ do
        it "handles variable declaration with new literal style" $ do
          let ast = Program
                [ TLExpr (VarDecl "x" (Lit (IntLit "5" (Just Int32))))
                , TLExpr (Call "print" [Var "x"])
                ]
          case convertToIR' ast of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case stmts of
                      [(declStmt, declMeta), (printStmt, printMeta), _] -> do
                          -- Check variable declaration
                          case declStmt of
                              IRVarDecl name irType expr -> do
                                  name `shouldBe` "x"
                                  irType `shouldBe` IRTypeInt32
                                  expr `shouldBe` IRLit (IRInt32Lit 5)
                                  metaLiteralType declMeta `shouldBe` Just (LitInt Int32)
                              _ -> expectationFailure "Expected variable declaration"

                          -- Check print statement
                          case printStmt of
                              IRProcCall "print" [IRVar "x"] ->
                                  return ()
                              _ -> expectationFailure "Expected print statement"
                      _ -> expectationFailure $ "Expected exactly two statements, got: " ++ show stmts
              Left err -> expectationFailure $ show err

      where
        testMeta = IRMetadata
          { metaType = IRTypeVar (TypeVar 0)  -- Use type variable instead of concrete type
          , metaEffects = S.singleton PureEffect
          , metaSourcePos = Nothing
          , metaLiteralType = Nothing
          }
