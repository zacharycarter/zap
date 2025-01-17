{-# LANGUAGE OverloadedStrings #-}
module Zap.IRSpec (spec
  , mkTestStructs
  , testStruct
  ) where

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

import Zap.AST
import Zap.IR
import Zap.Analysis.Semantic (parseSymTable)

-- Helper to create a test struct type
mkTestStruct :: String -> [(String, Type)] -> (Type, SymbolTable)
mkTestStruct name fields =
    let st = emptySymbolTable
        (sid, st') = registerStruct name fields st
    in (TypeStruct sid name, st')

-- Helper to create multiple test structs
mkTestStructs :: [(String, [(String, Type)])] -> ([(String, Type)], SymbolTable)
mkTestStructs defs =
    let (types, finalSt) = foldr addStruct ([], emptySymbolTable) defs
    in (types, finalSt)
  where
    addStruct (name, fields) (acc, st) =
        let (sid, st') = registerStruct name fields st
        in ((name, TypeStruct sid name):acc, st')

-- Test smart constructor
testStruct :: String -> [(String, Type)] -> Type
testStruct name fields =
    let (typ, _) = mkTestStruct name fields
    in typ

makeTestSymbolTable :: Program -> SymbolTable
makeTestSymbolTable prog@(Program _) =
    case parseSymTable prog of
        Just st -> st
        Nothing -> emptySymbolTable

spec :: Spec
spec = do
  describe "IR Conversion" $ do
    describe "Basic Program Structure" $ do
      it "creates a main function for top-level expressions" $ do
        let ast = Program [TLExpr (Call "print" [Lit (StringLit "test")])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
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
        let ast = Program [TLExpr (Call "print" [Lit (StringLit "test")])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case last stmts of
              (IRReturn Nothing, meta) -> do
                metaType meta `shouldBe` IRTypeVoid
                metaEffects meta `shouldBe` S.singleton PureEffect
              _ -> expectationFailure "Expected implicit return"
          Right _ -> expectationFailure $ "Should've received IRProgram type"
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

      it "converts if expressions" $ do
        let ast = Program [TLExpr (If
                  (BinOp Eq (Var "x") (Lit (IntLit "0" (Just Int64))))
                  (Block "then" [Lit (IntLit "1" (Just Int64))] Nothing)
                  (Block "else" [Lit (IntLit "2" (Just Int64))] Nothing))]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
            Right (IRProgram [(mainFn, _)]) -> do
                -- Should generate labels and conditional jumps
                let IRBlock _ stmts = fnBody mainFn
                case stmts of
                    [(IRJumpIfZero _ label1, _),        -- Skip then branch if false
                     (_thenStmt, _),                     -- Then branch
                     (IRGoto label2, _),                -- Skip else branch
                     (IRLabel elseLabel, _),            -- Else branch label
                     (_elseStmt, _),
                     (IRLabel endLabel, _),             -- End label
                     _] -> do
                        label1 `shouldBe` elseLabel     -- Jump to else if condition false
                        endLabel `shouldBe` label2      -- Both branches continue here
                    _ -> expectationFailure $ "Expected if/then/else structure, got: " ++ show stmts
            Right _ -> expectationFailure $ "Should've received IRProgram type"
            Left err -> expectationFailure $ show err

      it "converts recursive function with if expression" $ do
        let func = IRFuncDecl
              { fnName = "factorial"
              , fnParams = [("n", IRTypeInt32)]
              , fnRetType = IRTypeInt32
              , fnBody = IRBlock "entry"
                  [(IRReturn (Just (IRCall "factorial" [IRLit (IRInt32Lit 5)])), testMeta)]
              }
        let prog = IRProgram [(func, testMeta)]

        case generateConstraints prog of
          Right constraints -> do
            constraints `shouldContain` [TEq IRTypeInt32 IRTypeInt32]  -- Return type matches
          Left err -> expectationFailure $ show err

      it "converts factorial function if/else with returns" $ do
        -- We build a small AST for:
        -- factorial(n) = if (n == 0) 1 else n * factorial(n - 1)
        let factorialAST = DFunc "factorial"
              []
              [Param "n" (TypeNum Int32)]
              (TypeNum Int32)
              (Block "function_body"
                [ If (BinOp Eq (Var "n") (Lit (IntLit "0" (Just Int32))))
                     (Block "if_then" [Lit (IntLit "1" (Just Int32))] Nothing)
                     (Block "if_else"
                       [ BinOp Mul
                           (Var "n")
                           (Call "factorial"
                             [ BinOp Sub
                                 (Var "n")
                                 (Lit (IntLit "1" (Just Int32)))
                             ])
                       ] Nothing)
                ]
                Nothing
              )
        let eitherResult = convertFuncDecl emptySymbolTable factorialAST

        case eitherResult of
          Left err -> expectationFailure $
            "Expected successful IR conversion, but got: " ++ show err
          Right (irFunc, _meta) -> do
            let IRBlock _ stmts = fnBody irFunc

            case stmts of
              [ (IRJumpIfZero _cond lblElse, _),
                (IRReturn (Just (IRLit (IRInt32Lit 1))), _),
                (IRLabel lblElse', _),
                (IRReturn (Just (IRCall "Mul" [IRVar "n", IRCall "factorial" [IRCall "Sub" [IRVar "n", IRLit (IRInt32Lit 1)]]])), _) ]
                  -> do
                    lblElse `shouldBe` "if_else"
                    lblElse' `shouldBe` "if_else"
                    -- The "short" scenario is correct: no final label needed.
                    return ()

              other -> expectationFailure $
                "Expected if/else structure with returns; got IR statements:\n  " ++ show other

    describe "Error Handling" $ do
      it "reports error for unsupported expressions" $ do
        let ast = Program [TLExpr (Call "unknown" [])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Left (IRUnsupportedExpr _) -> return ()
          Left err -> expectationFailure $ "Expected unsupported expr error, got: " ++ show err
          Right _ -> expectationFailure "Expected error for unsupported expression"

      it "reports error for print with no arguments" $ do
        let ast = Program [TLExpr (Call "print" [])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Left (IRInvalidFunction _) -> return ()
          Left err -> expectationFailure $ "Expected invalid function error, got: " ++ show err
          Right _ -> expectationFailure "Expected error for invalid print call"

    describe "IR Metadata" $ do
      it "tracks IOEffect for print statements" $ do
        let ast = Program [TLExpr (Call "print" [Lit (StringLit "test")])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram [(mainFn, fnMeta)]) -> do
            -- Test function metadata
            metaEffects fnMeta `shouldBe` S.singleton IOEffect

            -- Test statement metadata directly from the function body
            let IRBlock _ stmts = fnBody mainFn
            case stmts !! 0 of
              (_, stmtMeta) -> metaEffects stmtMeta `shouldBe` S.singleton IOEffect
          Right _ -> expectationFailure $ "Should've received IRProgram type"
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
          let ast = Program [TLExpr (Call "print" [Lit (StringLit "test")])]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case stmts !! 0 of
                      (IRProcCall "print" [IRLit (IRStringLit "test")], _) -> return ()
                      other -> expectationFailure $ "Expected procedure call, got: " ++ show other
              Right _ -> expectationFailure $ "Should've received IRProgram"
              Left err -> expectationFailure $ show err

      it "converts print with binary operation to procedure call" $ do
          let ast = Program [TLExpr (Call "print" [BinOp Add (Lit (IntLit "1" (Just Int32))) (Lit (IntLit "2" (Just Int32)))])]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case stmts !! 0 of
                      (IRProcCall "print" [IRLit (IRInt32Lit 3)], _) -> return ()
                      other -> expectationFailure $ "Expected procedure call, got: " ++ show other
              Right _ -> expectationFailure $ "Should've received IRProgram"
              Left err -> expectationFailure $ show err
    it "converts print to procedure call" $ do
      let ast = Program [TLExpr (Call "print" [Lit (StringLit "test")])]
      let st = makeTestSymbolTable ast
      case convertToIR' ast st of
        Right (IRProgram [(mainFn, _)]) -> do
          let IRBlock _ stmts = fnBody mainFn
          case stmts !! 0 of
            (IRProcCall "print" [IRLit (IRStringLit "test")], _) -> return ()
            _ -> expectationFailure "Expected procedure call"
        Right _ -> expectationFailure $ "Should've received IRProgram"
        Left err -> expectationFailure $ show err

    describe "Print statement conversion" $ do
      it "converts string literal print to procedure call" $ do
          let ast = Program [TLExpr (Call "print" [Lit (StringLit "test")])]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case stmts !! 0 of
                      (IRProcCall "print" [IRLit (IRStringLit "test")], _) -> return ()
                      other -> expectationFailure $ "Expected procedure call, got: " ++ show other
              Right _ -> expectationFailure $ "Should've received IRProgram"
              Left err -> expectationFailure $ show err

      it "converts print with binary operation to procedure call" $ do
          let ast = Program [TLExpr (Call "print" [BinOp Add (Lit (IntLit "1" (Just Int32))) (Lit (IntLit "2" (Just Int32)))])]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case stmts !! 0 of
                      (IRProcCall "print" [IRLit (IRInt32Lit 3)], _) -> return ()
                      other -> expectationFailure $ "Expected procedure call, got: " ++ show other
              Right _ -> expectationFailure $ "Should've received IRProgram"
              Left err -> expectationFailure $ show err

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
          let _irInt64 = IRLit (IRInt64Lit 1)
          let _irFloat32 = IRLit (IRFloat32Lit 1.0)
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
                      Left err -> expectationFailure $ "Constraint solving failed: " ++ show err
              Left err -> expectationFailure $ show err

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
        let pointFields = [("x", TypeNum Float32), ("y", TypeNum Float32)]
        let (pointType, _symTable) = mkTestStruct "Point" pointFields

        let ast = Program
              [ TLType "Point" pointType
              , TLExpr (Let "p" (StructLit "Point" [("x", Lit (FloatLit "1.0" (Just Float32))),
                                                   ("y", Lit (FloatLit "2.0" (Just Float32)))]))
              , TLExpr (Call "print" [FieldAccess (Var "p") "x"])
              ]

        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram funcs) -> do
            length funcs `shouldBe` 1  -- Should be main function
            -- Rest of test assertions...
          Left err -> expectationFailure $ "IR conversion failed: " ++ show err

      describe "Literal conversion" $ do
        it "preserves literal type information in IR" $ do
          let ast = Program [TLExpr (Let "x" (Lit (IntLit "42" (Just Int32))))]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
            Right (IRProgram funcs) -> do
              case funcs of
                [(mainFn, _)] -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case stmts !! 0 of
                    (IRVarDecl (IRVarDeclData "x" _ (IRLit _lit)), meta) -> do
                      metaLiteralType meta `shouldBe` Just (LitInt Int32)
                    _ -> expectationFailure "Expected variable declaration"
                _ -> expectationFailure "IR conversion failed: "
            Left err -> expectationFailure $ "IR conversion failed: " ++ show err

        it "preserves float literal type information" $ do
          let ast = Program [TLExpr (Let "x" (Lit (FloatLit "3.14" (Just Float32))))]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
            Right (IRProgram [(mainFn, _)]) -> do
              case fnBody mainFn of
                IRBlock _ ((IRVarDecl (IRVarDeclData _ _ _), meta):_) ->
                  metaLiteralType meta `shouldBe` Just (LitFloat Float32)
                _ -> expectationFailure "Expected variable declaration"
            Right _ -> expectationFailure "Conversion expected IRProgram"
            Left err -> expectationFailure $ show err

        it "preserves string literal type information" $ do
          let ast = Program [TLExpr (Let "x" (Lit (StringLit "test")))]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
            Right (IRProgram [(mainFn, _)]) -> do
              case fnBody mainFn of
                IRBlock _ ((IRVarDecl (IRVarDeclData _ _ _), meta):_) ->
                  metaLiteralType meta `shouldBe` Just LitString
                _ -> expectationFailure "Expected variable declaration"
            Right _ -> expectationFailure "Conversion expected IRProgram"
            Left err -> expectationFailure $ show err

        it "preserves numeric types" $ do
            let ast = Program [TLExpr (Call "print" [Lit (FloatLit "3.14" (Just Float32))])]
            let st = makeTestSymbolTable ast
            case convertToIR' ast st of
                Right (IRProgram [(mainFn, _)]) -> do
                    let IRBlock _ ((IRProcCall "print" [IRLit lit], _):_) = fnBody mainFn
                    lit `shouldBe` IRFloat32Lit 3.14
                Right _ -> expectationFailure "Conversion expected IRProgram"
                Left err -> expectationFailure $ show err

      describe "Variable declarations" $ do
        it "handles variable declaration with new literal style" $ do
          let ast = Program
                [ TLExpr (VarDecl "x" (Lit (IntLit "5" (Just Int32))))
                , TLExpr (Call "print" [Var "x"])
                ]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
              Right (IRProgram [(mainFn, _)]) -> do
                  let IRBlock _ stmts = fnBody mainFn
                  case stmts of
                      [(declStmt, declMeta), (printStmt, _printMeta), _] -> do
                          -- Check variable declaration
                          case declStmt of
                              IRVarDecl (IRVarDeclData name irType expr) -> do
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
              Right _ -> expectationFailure "Conversion expected IRProgram"
              Left err -> expectationFailure $ show err

    describe "Generic type handling" $ do
      it "converts generic struct definition to IR" $ do
        let input = Program [ TLType "Box" $ TypeStruct (StructId 0) "Box"
                            , TLExpr $ Let "x" $ Call "Box" [Lit (IntLit "42" (Just Int32))]
                            ]
        let st = makeTestSymbolTable input
        case convertToIR' input st of
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case stmts !! 0 of
              (IRVarDecl (IRVarDeclData "x" (IRTypeStruct "Box_i32" _) _), _) -> return ()
              other -> expectationFailure $ "Expected concrete Box instantiation, got: " ++ show other
          Right _ -> expectationFailure "Conversion expected IRProgram"
          Left err -> expectationFailure $ show err

      it "propagates concrete type information through variable declarations" $ do
        let input = Program [ TLType "Box" $ TypeStruct (StructId 0) "Box"
                           , TLExpr $ Let "x" $ Call "Box[i32]" [Lit (IntLit "42" (Just Int32))]
                           , TLExpr $ Call "print" [FieldAccess (Var "x") "value"]
                           ]
        let st = makeTestSymbolTable input
        case convertToIR' input st of
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case stmts of
              [ (IRVarDecl (IRVarDeclData "x" (IRTypeStruct "Box_i32" _) declInit), _declMeta),
                (IRProcCall "print" [IRCall "field_access" [IRVar "x", IRLit (IRStringLit "value")]], _printMeta),
                (IRReturn Nothing, _) ] -> do
                  case declInit of
                    IRCall "struct_lit" [IRLit (IRStringLit "Box_i32"), _] -> return ()
                    other -> expectationFailure $
                      "Expected struct_lit with concrete type, got: " ++ show other
              other -> expectationFailure $
                "Expected variable declaration, print and return statements, got: " ++ show other
          Right _ -> expectationFailure "Conversion expected IRProgram"
          Left err -> expectationFailure $ show err

      it "preserves base struct name when no type parameter given" $ do
        let ast = Call "Point"
                    [Lit (IntLit "10" (Just Int64)),
                     Lit (IntLit "20" (Just Int64))]
        case convertToIRExpr ast of
          Right (IRCall "struct_lit"
                  (IRLit (IRStringLit "Point"):_)) -> return ()
          other -> expectationFailure $
            "Expected basic struct_lit call, got: " ++ show other

      it "uses specialized name with explicit type parameter" $ do
        -- First register the base struct
        let (_baseId, symTable) = registerStruct "Box"
                                  [("value", TypeNum Int32)]
                                  emptySymbolTable
        let ast = Call "Box[i32]"
                      [Lit (IntLit "42" (Just Int32))]
        case convertToIRExprWithSymbols symTable ast of
          Right (IRCall "struct_lit"
                  (IRLit (IRStringLit "Box_i32"):_)) -> return ()
          other -> expectationFailure $
            "Expected specialized struct_lit call, got: " ++ show other

      where
        testMeta = IRMetadata
          { metaType = IRTypeVar (TypeVar 0)  -- Use type variable instead of concrete type
          , metaEffects = S.singleton PureEffect
          , metaSourcePos = Nothing
          , metaLiteralType = Nothing
          , metaSymTable = Nothing
          }
