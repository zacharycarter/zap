{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.ParserSpec (spec) where

import Test.Hspec
import Data.Either (isLeft)
import qualified Data.Text as T
import Debug.Trace

import Zap.Parser.Core (ParseError(..))
import Zap.Parser.Expr
import Zap.Parser.Program
import Zap.AST

extractParseResult :: Either ParseError ([TopLevel], SymbolTable) -> Either ParseError [TopLevel]
extractParseResult = fmap fst

-- Helper for tests that just care about the AST
expectParseSuccess :: T.Text -> ([TopLevel] -> IO ()) -> IO ()
expectParseSuccess input validate = case parseProgram input of
    Right (tops, _) -> validate tops
    Left err -> expectationFailure $ "Parse failed: " ++ show err

-- Helper for tests that need symbol table access
expectParseWithSymbols :: T.Text -> (([TopLevel], SymbolTable) -> IO ()) -> IO ()
expectParseWithSymbols input validate = case parseProgram input of
    Right result -> validate result
    Left err -> expectationFailure $ "Parse failed: " ++ show err

spec :: Spec
spec = do
  describe "Print statement parsing" $ do
    it "parses traditional print syntax" $ do
      let input = "print \"Hello, World!\""
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Call "print" [Lit (StringLit str)])] ->
          str `shouldBe` "Hello, World!"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "parses function-style print syntax" $ do
      let input = "print(\"Hello, World!\")"
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Call "print" [Lit (StringLit str)])] ->
          str `shouldBe` "Hello, World!"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "parses print with complex expression" $ do
      expectParseSuccess "print(1 + 2 * 3)" $ \tops ->
        tops `shouldBe` [TLExpr (Call "print" [
            BinOp Add
                (Lit (IntLit "1" (Just Int64)))
                (BinOp Mul (Lit (IntLit "2" (Just Int64))) (Lit (IntLit "3" (Just Int64))))])]

    it "enforces print statement indentation" $ do
      let input = "block test:\nprint \"Hello\""  -- Not indented
      parseProgram input `shouldSatisfy` isLeft

  describe "Block parsing" $ do
    it "parses simple block" $ do
      let input = "block test:\n  \"Hello\""
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Block blockLabel blockExprs blockResult)] -> do
          blockLabel `shouldBe` "test"
          blockExprs `shouldBe` [Lit (StringLit "Hello")]
          blockResult `shouldBe` Nothing
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "enforces block indentation" $ do
      let input = "block test:\n\"Hello\""  -- Not indented
      parseProgram input `shouldSatisfy` isLeft

    it "parses nested blocks" $ do
      let input = "block outer:\n  block inner:\n    \"Hello\""
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Block blockLabel blockExprs blockResult)] -> do
          blockLabel `shouldBe` "outer"
          case blockExprs of
            (Block innerBlockLabel innerBlockExprs _:_) -> do
              innerBlockLabel `shouldBe` "inner"
              innerBlockExprs `shouldBe` [Lit (StringLit "Hello")]
            _ -> expectationFailure "Expected inner block"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    -- Rest of the test cases follow similar pattern...

  describe "Top-level expressions" $ do
    it "parses multiple top-level expressions" $ do
      let input = T.unlines
            [ "print \"First\""
            , "block test:"
            , "  print \"Second\""
            ]
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Call "print" [Lit (StringLit first)]), TLExpr (Block _ blockExprs _)] -> do
          first `shouldBe` "First"
          blockExprs `shouldBe` [Call "print" [Lit (StringLit "Second")]]
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

  describe "While loop parsing" $ do
    it "parses simple while loop" $ do
      let input = "while x < 3:\n  print x"
      case extractParseResult $ parseProgram input of
        Right [TLExpr (While (BinOp Lt (Var "x") (Lit (IntLit "3" (Just Int64)))) (Block _ blockExprs _))] ->
          blockExprs `shouldBe` [Call "print" [Var "x"]]
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other


    it "enforces proper while loop structure" $ do
      let input = "while x < 3"  -- Missing colon
      parseProgram input `shouldSatisfy` isLeft

  describe "Numeric literal parsing" $ do
    it "parses integer literals as Int64" $ do
      parseExprFromText "42'i64" `shouldBe` Right (Lit (IntLit "42" (Just Int64)))

    it "parses decimal literals as Float64" $ do
      parseExprFromText "42.0'f64" `shouldBe` Right (Lit (FloatLit "42.0" (Just Float64)))

  describe "Variable declarations and assignments" $ do
    it "parses variable declaration with initialization" $ do
        parseExprFromText "var x = 42" `shouldBe`
            Right (VarDecl "x" (Lit (IntLit "42" (Just Int64))))

    it "parses variable declaration within block" $ do
        expectParseSuccess "block test:\n  var x = 42" $ \tops ->
          tops `shouldBe` [TLExpr (Block
                "test"
                [VarDecl "x" (Lit (IntLit "42" (Just Int64)))]
                Nothing)]

    it "parses variable assignment" $ do
        parseExprFromText "x = 42" `shouldBe`
            Right (Assign "x" (Lit (IntLit "42" (Just Int64))))

    it "parses variable declaration and assignment in function" $ do
        let input = T.unlines
              [ "fn sum_squares(x, y: i32): i32 ="
              , "  var sum = x * x"
              , "  sum = sum + y * y"
              , "  sum"
              ]
        expectParseSuccess input $ \tops ->
          tops `shouldBe` [TLDecl (DFunc "sum_squares"
                [Param "x" (TypeNum Int32), Param "y" (TypeNum Int32)]
                (TypeNum Int32)
                (Block
                  "function_body"
                  [
                    VarDecl "sum" (BinOp Mul (Var "x") (Var "x"))
                  , Assign "sum" (BinOp Add (Var "sum") (BinOp Mul (Var "y") (Var "y")))
                  ]
                  (Just (Var "sum"))))]

  describe "Type consistency in numeric literals" $ do
    it "maintains consistent type information between literal and declared type" $ do
      let input = "let x: i32 = 42"  -- Variable with explicit type annotation
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Let "x" expr)] -> do
          -- The IntLit type should match the declared type
          case expr of
            Lit (IntLit val numType) -> do
              numType `shouldBe` (Just Int32)
              val `shouldBe` "42"
            _ -> expectationFailure "Expected numeric literal"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "infers consistent types for numeric literals" $ do
      let input = "42'i32"  -- Literal with type suffix
      case parseExprFromText input of
        Right (Lit (IntLit val numType)) -> do
          numType `shouldBe` (Just Int32)
          val `shouldBe` "42"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other -> expectationFailure $
          "Unexpected parse result: " ++ show other

    it "handles literals with contextual type information" $ do
      let input = "let x: i32 = 42\nlet y: i64 = x"
      case extractParseResult $ parseProgram input of
        Right [first, second] -> do
          case first of
            TLExpr (Let "x" (Lit (IntLit "42" (Just Int32)))) -> return ()  -- Accept new form
            other -> expectationFailure $ "Unexpected first expr: " ++ show other
          second `shouldBe` TLExpr (Let "y" (Var "x"))
        other -> expectationFailure $ "Unexpected parse result: " ++ show other

    it "enforces type consistency with float literals" $ do
      let input = "let x: f32 = 3.14"
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Let "x" (Lit (FloatLit "3.14" (Just Float32))))] -> return ()
        other -> expectationFailure $ "Unexpected parse result: " ++ show other

    it "parses struct definition with fields" $ do
      expectParseWithSymbols "type Point = struct\n  x: i32\n  y: i32" $ \(tops, st) -> do
          case tops of
              [TLType name typ@(TypeStruct sid _)] -> do
                  name `shouldBe` "Point"
                  case lookupStruct sid st of
                      Just def -> do
                          structName def `shouldBe` "Point"
                          structFields def `shouldBe` [("x", TypeNum Int32), ("y", TypeNum Int32)]
                      Nothing -> expectationFailure "Struct not found in symbol table"
              _ -> expectationFailure $ "Unexpected parse result: " ++ show tops

    it "registers struct in symbol table during parse" $ do
      let input = T.unlines [ "type Point = struct"
                            , "  x: i32"
                            , "  y: i32"
                            ]
      expectParseWithSymbols "type Point = struct\n  x: i32\n  y: i32" $ \(tops, symTable) ->
        case tops of
          [TLType name typ@(TypeStruct sid _)] -> do
            name `shouldBe` "Point"
            -- Check the struct is properly registered
            lookupStruct sid symTable `shouldNotBe` Nothing
          _ -> expectationFailure "Expected struct type definition"

    it "parses struct definition with type parameters" $ do
      let input = T.unlines [ "type Box[T] = struct"
                           , "  value: T"
                           ]
      expectParseWithSymbols input $ \(tops, st) -> do
          case tops of
              [TLType name typ@(TypeStruct sid _)] -> do
                  name `shouldBe` "Box"
                  case lookupStruct sid st of
                      Just def -> do
                          structName def `shouldBe` "Box"
                          structParams def `shouldBe` ["T"]
                          structFields def `shouldBe` [("value", TypeParam "T")]
                      Nothing -> expectationFailure "Struct not found in symbol table"
              _ -> expectationFailure $ "Unexpected parse result: " ++ show tops

    it "parses type parameter in field declaration" $ do
      let input = T.unlines [ "type Box[T] = struct"
                           , "  value: T"
                           ]
      expectParseWithSymbols input $ \(tops, st) -> do
          case tops of
              [TLType name typ@(TypeStruct sid _)] -> do
                  name `shouldBe` "Box"
                  case lookupStruct sid st of
                      Just def -> do
                          structParams def `shouldBe` ["T"]
                          structFields def `shouldBe` [("value", TypeParam "T")]
                      Nothing -> expectationFailure "Struct not found in symbol table"
              _ -> expectationFailure $ "Unexpected parse result: " ++ show tops

    it "parses generic type instantiation" $ do
      expectParseSuccess "let x = Box[i32](42'i32)" $ \tops ->
          case tops of
              [TLExpr (Let "x" (Call "Box" [Lit (IntLit "42" (Just Int32))]))] -> return ()
              other -> expectationFailure $
                  "Expected generic box instantiation, got: " ++ show other

    it "parses generic type instantiation in expressions" $ do
      -- Test just the expression part, not top-level
      expectParseSuccess "Box[i32](42)" $ \tops ->
          case tops of
              [TLExpr expr] -> do
                  traceM $ "Got expression: " ++ show expr
                  expr `shouldBe` Call "Box" [Lit (IntLit "42" (Just Int64))]
              other -> expectationFailure $
                  "Expected top-level expression, got: " ++ show other
