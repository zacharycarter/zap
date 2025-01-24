{-# LANGUAGE OverloadedStrings #-}

module Zap.IRSpec (spec) where

import qualified Data.Set as S
import Test.Hspec
import Zap.AST
import Zap.Analysis.Semantic (parseSymTable)
import Zap.IR

-- Helper unchanged
mkTestStruct :: String -> [(String, Type)] -> (Type, SymbolTable)
mkTestStruct name fields =
  let st = emptySymbolTable
      (sid, st') = registerStruct name fields st
   in (TypeStruct sid name, st')

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
          Right (IRProgram []) ->
            expectationFailure "Expected single main function, got empty program"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected single main function, got multiple functions"
          Right (IRProgram [(mainFn, meta)]) -> do
            fnName mainFn `shouldBe` "main"
            fnParams mainFn `shouldBe` []
            fnRetType mainFn `shouldBe` IRTypeVoid
            metaType meta `shouldBe` IRTypeVoid
            metaEffects meta `shouldBe` S.singleton IOEffect
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

      it "adds implicit return to main block" $ do
        let ast = Program [TLExpr (Call "print" [Lit (StringLit "test")])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram []) ->
            expectationFailure "Expected one function, got none"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected one function, got multiple"
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case reverse stmts of
              [] -> expectationFailure "Expected at least one statement"
              ((IRReturn Nothing, meta) : _) -> do
                metaType meta `shouldBe` IRTypeVoid
                metaEffects meta `shouldBe` S.singleton PureEffect
              _ -> expectationFailure "Expected implicit return as last statement"
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

      it "converts if expressions" $ do
        let ast =
              Program
                [ TLExpr
                    ( If
                        (BinOp Eq (Var "x") (Lit (IntLit "0" (Just Int64))))
                        (Block "then" [Lit (IntLit "1" (Just Int64))] Nothing)
                        (Block "else" [Lit (IntLit "2" (Just Int64))] Nothing)
                    )
                ]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram []) ->
            expectationFailure "Expected one function, got none"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected one function, got multiple"
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case stmts of
              [ (IRJumpIfZero _ label1, _),
                (_, _),
                (IRGoto label2, _),
                (IRLabel elseLabel, _),
                (_, _),
                (IRLabel endLabel, _),
                _
                ] -> do
                  label1 `shouldBe` elseLabel
                  endLabel `shouldBe` label2
              _ -> expectationFailure $ "Expected if/then/else structure, got: " ++ show stmts
          Left err -> expectationFailure $ show err

      it "converts factorial function if/else with returns" $ do
        let factorialAST =
              DFunc
                "factorial"
                []
                [Param "n" (TypeNum Int32)]
                (TypeNum Int32)
                ( Block
                    "function_body"
                    [ If
                        (BinOp Eq (Var "n") (Lit (IntLit "0" (Just Int32))))
                        (Block "if_then" [Lit (IntLit "1" (Just Int32))] Nothing)
                        ( Block
                            "if_else"
                            [ BinOp
                                Mul
                                (Var "n")
                                ( Call
                                    "factorial"
                                    [ BinOp
                                        Sub
                                        (Var "n")
                                        (Lit (IntLit "1" (Just Int32)))
                                    ]
                                )
                            ]
                            Nothing
                        )
                    ]
                    Nothing
                )
        let eitherResult = convertFuncDecl emptySymbolTable factorialAST

        case eitherResult of
          Left err ->
            expectationFailure $
              "Expected successful IR conversion, but got: " ++ show err
          Right (irFunc, _meta) -> do
            let IRBlock _ stmts = fnBody irFunc
            case stmts of
              [ (IRJumpIfZero _cond lblElse, _),
                (IRReturn (Just (IRLit (IRInt32Lit 1))), _),
                (IRLabel lblElse', _),
                (IRReturn (Just (IRCall "Mul" [IRVar "n", IRCall "factorial" [IRCall "Sub" [IRVar "n", IRLit (IRInt32Lit 1)]]])), _)
                ] ->
                  do
                    lblElse `shouldBe` "if_else"
                    lblElse' `shouldBe` "if_else"
                    return ()
              other ->
                expectationFailure $
                  "Expected if/else structure with returns; got IR statements:\n  " ++ show other

      it "converts breaks to appropriate IR constructs" $ do
        -- Test function-level break turns into IRReturn
        let funcBreak = Break (Just "myFunc") (Just (Lit (IntLit "42" (Just Int32))))
        let ast1 = Program [TLExpr funcBreak]
        let st = makeTestSymbolTable ast1
        case convertToIR' ast1 st of
          Right (IRProgram []) ->
            expectationFailure "Expected one function, got none"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected one function, got multiple"
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case stmts of
              [(IRReturn (Just (IRLit (IRInt32Lit 42))), _)] -> return ()
              other ->
                expectationFailure $
                  "Expected single IRReturn, got: " ++ show other
          Left err -> expectationFailure $ show err

        -- Test loop break stops loop control flow
        let loopBreak =
              Program
                [ TLExpr
                    ( While
                        (BinOp Lt (Var "x") (Lit (IntLit "10" (Just Int32))))
                        ( Block
                            "loop_body"
                            [Break (Just "while_0_end") Nothing]
                            Nothing
                        )
                    )
                ]
        let st2 = makeTestSymbolTable loopBreak
        case convertToIR' loopBreak st2 of
          Right (IRProgram []) ->
            expectationFailure "Expected one function, got none"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected one function, got multiple"
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case stmts of
              [ (IRLabel start, _),
                (IRJumpIfZero _ end1, _),
                (IRGoto end2, _),
                (IRLabel end3, _),
                (IRReturn Nothing, _)
                ]
                  | start == "while_0_start"
                      && end1 == "while_0_end"
                      && end2 == "while_0_end"
                      && end3 == "while_0_end" ->
                      return ()
              other ->
                expectationFailure $
                  "Expected loop with break pattern, got: " ++ show other
          Left err -> expectationFailure $ show err

    describe "Error Handling" $ do
      it "reports error for unsupported expressions" $ do
        let ast = Program [TLExpr (Call "unknown" [])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right _ -> expectationFailure "Expected error for unsupported expression"
          Left (IRUnsupportedExpr _) -> return ()
          Left err -> expectationFailure $ "Expected unsupported expr error, got: " ++ show err

      it "reports error for print with no arguments" $ do
        let ast = Program [TLExpr (Call "print" [])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right _ -> expectationFailure "Expected error for invalid print call"
          Left (IRInvalidFunction _) -> return ()
          Left err -> expectationFailure $ "Expected invalid function error, got: " ++ show err

    describe "IR Metadata" $ do
      it "tracks IOEffect for print statements" $ do
        let ast = Program [TLExpr (Call "print" [Lit (StringLit "test")])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram []) ->
            expectationFailure "Expected one function, got none"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected one function, got multiple"
          Right (IRProgram [(mainFn, fnMeta)]) -> do
            metaEffects fnMeta `shouldBe` S.singleton IOEffect
            let IRBlock _ stmts = fnBody mainFn
            case stmts of
              [] -> expectationFailure "Expected at least one statement"
              ((_, stmtMeta) : _) ->
                metaEffects stmtMeta `shouldBe` S.singleton IOEffect
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

    describe "Print statement conversion" $ do
      it "converts string literal print to procedure call" $ do
        let ast = Program [TLExpr (Call "print" [Lit (StringLit "test")])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram []) ->
            expectationFailure "Expected one function, got none"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected one function, got multiple"
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case stmts of
              [] -> expectationFailure "Expected at least one statement"
              ((IRProcCall "print" [IRLit (IRStringLit "test")], _) : _) -> return ()
              (other : _) -> expectationFailure $ "Expected procedure call, got: " ++ show other
          Left err -> expectationFailure $ "Conversion failed: " ++ show err

      it "converts print with binary operation to procedure call" $ do
        let ast = Program [TLExpr (Call "print" [BinOp Add (Lit (IntLit "1" (Just Int32))) (Lit (IntLit "2" (Just Int32)))])]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram []) ->
            expectationFailure "Expected one function, got none"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected one function, got multiple"
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case stmts of
              [] -> expectationFailure "Expected at least one statement"
              ((IRProcCall "print" [IRLit (IRInt32Lit 3)], _) : _) -> return ()
              (other : _) -> expectationFailure $ "Expected procedure call, got: " ++ show other
          Left err -> expectationFailure $ show err

    describe "Variable declarations" $ do
      it "handles variable declaration with new literal style" $ do
        let ast =
              Program
                [ TLExpr (VarDecl "x" (Lit (IntLit "5" (Just Int32)))),
                  TLExpr (Call "print" [Var "x"])
                ]
        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram []) ->
            expectationFailure "Expected one function, got none"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected one function, got multiple"
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case stmts of
              ((declStmt, declMeta) : (printStmt, _) : _) -> do
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
              _ -> expectationFailure $ "Expected at least two statements"
          Left err -> expectationFailure $ show err

    describe "Struct type handling" $ do
      it "converts struct definitions to IR" $ do
        let pointFields = [("x", TypeNum Float32), ("y", TypeNum Float32)]
        let (pointType, _) = mkTestStruct "Point" pointFields

        let ast =
              Program
                [ TLType "Point" pointType,
                  TLExpr
                    ( Let
                        "p"
                        ( StructLit
                            "Point"
                            [ ("x", Lit (FloatLit "1.0" (Just Float32))),
                              ("y", Lit (FloatLit "2.0" (Just Float32)))
                            ]
                        )
                    ),
                  TLExpr (Call "print" [FieldAccess (Var "p") "x"])
                ]

        let st = makeTestSymbolTable ast
        case convertToIR' ast st of
          Right (IRProgram []) ->
            expectationFailure "Expected one function, got none"
          Right (IRProgram ((_, _) : _ : _)) ->
            expectationFailure "Expected one function, got multiple"
          Right (IRProgram [(mainFn, _)]) -> do
            let IRBlock _ stmts = fnBody mainFn
            case stmts of
              [] -> expectationFailure "Expected statements in block"
              _ -> return () -- Basic structure verification
          Left err -> expectationFailure $ "IR conversion failed: " ++ show err

      describe "Literal conversion" $ do
        it "preserves literal type information in IR" $ do
          let ast = Program [TLExpr (Let "x" (Lit (IntLit "42" (Just Int32))))]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
            Right (IRProgram []) ->
              expectationFailure "Expected one function, got none"
            Right (IRProgram ((_, _) : _ : _)) ->
              expectationFailure "Expected one function, got multiple"
            Right (IRProgram [(mainFn, _)]) -> do
              let IRBlock _ stmts = fnBody mainFn
              case stmts of
                [] -> expectationFailure "Expected at least one statement"
                ((IRVarDecl "x" _ (IRLit _), meta) : _) ->
                  metaLiteralType meta `shouldBe` Just (LitInt Int32)
                _ -> expectationFailure "Expected variable declaration"
            Left err -> expectationFailure $ "IR conversion failed: " ++ show err

        it "preserves float literal type information" $ do
          let ast = Program [TLExpr (Let "x" (Lit (FloatLit "3.14" (Just Float32))))]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
            Right (IRProgram []) ->
              expectationFailure "Expected one function, got none"
            Right (IRProgram ((_, _) : _ : _)) ->
              expectationFailure "Expected one function, got multiple"
            Right (IRProgram [(mainFn, _)]) -> do
              case fnBody mainFn of
                IRBlock _ [] ->
                  expectationFailure "Expected at least one statement"
                IRBlock _ ((IRVarDecl _ _ _, meta) : _) ->
                  metaLiteralType meta `shouldBe` Just (LitFloat Float32)
                IRBlock _ ((_, _) : _) ->
                  expectationFailure "Expected variable declaration"
            Left err -> expectationFailure $ show err

        it "preserves string literal type information" $ do
          let ast = Program [TLExpr (Let "x" (Lit (StringLit "test")))]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
            Right (IRProgram []) ->
              expectationFailure "Expected one function, got none"
            Right (IRProgram ((_, _) : _ : _)) ->
              expectationFailure "Expected one function, got multiple"
            Right (IRProgram [(mainFn, _)]) -> do
              case fnBody mainFn of
                IRBlock _ [] ->
                  expectationFailure "Expected at least one statement"
                IRBlock _ ((IRVarDecl _ _ _, meta) : _) ->
                  metaLiteralType meta `shouldBe` Just LitString
                IRBlock _ ((_, _) : _) ->
                  expectationFailure "Expected variable declaration"
            Left err -> expectationFailure $ show err

        it "preserves numeric types" $ do
          let ast = Program [TLExpr (Call "print" [Lit (FloatLit "3.14" (Just Float32))])]
          let st = makeTestSymbolTable ast
          case convertToIR' ast st of
            Right (IRProgram []) ->
              expectationFailure "Expected one function, got none"
            Right (IRProgram ((_, _) : _ : _)) ->
              expectationFailure "Expected one function, got multiple"
            Right (IRProgram [(mainFn, _)]) -> do
              case fnBody mainFn of
                IRBlock _ [] ->
                  expectationFailure "Expected at least one statement"
                IRBlock _ ((IRProcCall "print" [IRLit lit], _) : _) ->
                  lit `shouldBe` IRFloat32Lit 3.14
                IRBlock _ ((other, _) : _) ->
                  expectationFailure $ "Expected print call, got: " ++ show other
            Left err -> expectationFailure $ show err

      describe "Generic type handling" $ do
        it "preserves base struct name when no type parameter given" $ do
          let ast =
                Call
                  "Point"
                  [ Lit (IntLit "10" (Just Int64)),
                    Lit (IntLit "20" (Just Int64))
                  ]
          case convertToIRExpr ast of
            Right (IRCall "struct_lit" []) ->
              expectationFailure "Expected struct_lit with parameters"
            Right (IRCall name (IRLit (IRStringLit str) : _))
              | name == "struct_lit" && str == "Point" -> return ()
            Right other ->
              expectationFailure $
                "Expected struct_lit call, got: " ++ show other
            Left err ->
              expectationFailure $ "Conversion failed: " ++ show err
