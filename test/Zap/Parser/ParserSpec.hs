{-# LANGUAGE OverloadedStrings #-}

module Zap.Parser.ParserSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace
import Test.Hspec
import Zap.AST
import Zap.Parser.Core (ParseError (..))
import Zap.Parser.Program

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
        Right other ->
          expectationFailure $
            "Unexpected parse result: " ++ show other

    it "parses function-style print syntax" $ do
      let input = "print(\"Hello, World!\")"
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Call "print" [Lit (StringLit str)])] ->
          str `shouldBe` "Hello, World!"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other ->
          expectationFailure $
            "Unexpected parse result: " ++ show other

    it "parses print with complex expression" $ do
      expectParseSuccess "print(1 + 2 * 3)" $ \tops ->
        tops
          `shouldBe` [ TLExpr
                         ( Call
                             "print"
                             [ BinOp
                                 Add
                                 (Lit (IntLit "1" (Just Int32)))
                                 (BinOp Mul (Lit (IntLit "2" (Just Int32))) (Lit (IntLit "3" (Just Int32))))
                             ]
                         )
                     ]

    it "enforces print statement indentation" $ do
      let input = "block test:\nprint \"Hello\"" -- Not indented
      parseProgram input `shouldSatisfy` isLeft

  describe "Block parsing" $ do
    it "parses simple block" $ do
      let input = "block test:\n  \"Hello\""
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Block blockLabel blockExprs _)] -> do
          blockLabel `shouldBe` "test"
          blockExprs `shouldBe` [Lit (StringLit "Hello")]
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other ->
          expectationFailure $
            "Unexpected parse result: " ++ show other

    it "enforces block indentation" $ do
      let input = "block test:\n\"Hello\"" -- Not indented
      parseProgram input `shouldSatisfy` isLeft

    it "parses nested blocks" $ do
      let input = "block outer:\n  block inner:\n    \"Hello\""
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Block blockLabel blockExprs _)] -> do
          blockLabel `shouldBe` "outer"
          case blockExprs of
            (Block innerBlockLabel innerBlockExprs _ : _) -> do
              innerBlockLabel `shouldBe` "inner"
              innerBlockExprs `shouldBe` [Lit (StringLit "Hello")]
            _ -> expectationFailure "Expected inner block"
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other ->
          expectationFailure $
            "Unexpected parse result: " ++ show other

  describe "Top-level expressions" $ do
    it "parses multiple top-level expressions" $ do
      let input =
            T.unlines
              [ "print \"First\"",
                "block test:",
                "  print \"Second\""
              ]
      case extractParseResult $ parseProgram input of
        Right [TLExpr (Call "print" [Lit (StringLit first)]), TLExpr (Block _ blockExprs _)] -> do
          first `shouldBe` "First"
          blockExprs `shouldBe` [Call "print" [Lit (StringLit "Second")]]
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right other ->
          expectationFailure $
            "Unexpected parse result: " ++ show other

    it "preserves type parameters in field types during parsing" $ do
      let input =
            T.unlines
              [ "type",
                "  Box[T] = struct",
                "    value: T",
                "",
                "  Nested[T] = struct",
                "    inner: Box[T]",
                "    value: T"
              ]
      case parseProgram input of
        Right (tops, st) ->
          case tops of
            [TLType "Box" _, TLType name (TypeStruct sid _)] -> do
              name `shouldBe` "Nested"
              case lookupStruct sid st of
                Just def -> do
                  structParams def `shouldBe` [TypeParam "T"]
                  let innerType = lookup "inner" (structFields def)
                  innerType `shouldBe` Just (TypeStruct (StructId 0) "Box")
                Nothing ->
                  expectationFailure "Could not find struct definition"
            _ -> expectationFailure "Expected only two type declarations"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses struct definition with fields" $ do
      expectParseWithSymbols "type Point = struct\n  x: i32\n  y: i32" $ \(tops, st) -> do
        case tops of
          [TLType name (TypeStruct sid _)] -> do
            name `shouldBe` "Point"
            case lookupStruct sid st of
              Just def -> do
                structName def `shouldBe` "Point"
                structFields def `shouldBe` [("x", TypeNum Int32), ("y", TypeNum Int32)]
              Nothing -> expectationFailure "Struct not found in symbol table"
          _ -> expectationFailure $ "Unexpected parse result: " ++ show tops

    it "registers variable type annotations in symbol table" $ do
      let input =
            T.unlines
              [ "let x: i32 = 42",
                "let y = x" -- Should be able to look up x's type
              ]
      expectParseWithSymbols input $ \(_, symTable) -> do
        lookupVarType "x" symTable `shouldBe` Just (TypeNum Int32)

    it "registers struct in symbol table during parse" $ do
      expectParseWithSymbols "type Point = struct\n  x: i32\n  y: i32" $ \(tops, symTable) ->
        case tops of
          [TLType name (TypeStruct sid _)] -> do
            name `shouldBe` "Point"
            -- Check the struct is properly registered
            lookupStruct sid symTable `shouldNotBe` Nothing
          _ -> expectationFailure "Expected struct type definition"

    it "parses struct definition with type parameters" $ do
      let input =
            T.unlines
              [ "type Box[T] = struct",
                "  value: T"
              ]
      expectParseWithSymbols input $ \(tops, st) -> do
        case tops of
          [TLType name (TypeStruct sid _)] -> do
            name `shouldBe` "Box"
            case lookupStruct sid st of
              Just def -> do
                structName def `shouldBe` "Box"
                structParams def `shouldBe` [TypeParam "T"]
                structFields def `shouldBe` [("value", TypeParam "T")]
              Nothing -> expectationFailure "Struct not found in symbol table"
          _ -> expectationFailure $ "Unexpected parse result: " ++ show tops

    it "parses type parameter in field declaration" $ do
      let input =
            T.unlines
              [ "type Box[T] = struct",
                "  value: T"
              ]
      expectParseWithSymbols input $ \(tops, st) -> do
        case tops of
          [TLType name (TypeStruct sid _)] -> do
            name `shouldBe` "Box"
            case lookupStruct sid st of
              Just def -> do
                structParams def `shouldBe` [TypeParam "T"]
                structFields def `shouldBe` [("value", TypeParam "T")]
              Nothing -> expectationFailure "Struct not found in symbol table"
          _ -> expectationFailure $ "Unexpected parse result: " ++ show tops

    it "parses generic type instantiation" $ do
      expectParseSuccess "let x = Box[i32](42'i32)" $ \tops ->
        case tops of
          [TLExpr (Let "x" (Call "Box_i32" [Lit (IntLit "42" (Just Int32))]))] -> return ()
          other ->
            expectationFailure $
              "Expected generic box instantiation, got: " ++ show other

    it "parses generic type instantiation in expressions" $ do
      -- Test just the expression part, not top-level
      expectParseSuccess "Box[i32](42)" $ \tops ->
        case tops of
          [TLExpr expr] -> do
            traceM $ "Got expression: " ++ show expr
            expr `shouldBe` Call "Box_i32" [Lit (IntLit "42" (Just Int32))]
          other ->
            expectationFailure $
              "Expected top-level expression, got: " ++ show other

    it "registers specialized struct in symbol table" $ do
      let specializedStructInput =
            T.unlines
              [ "type Box[T] = struct",
                "  value: T",
                "let b = Box[i32](42)"
              ]
      expectParseWithSymbols specializedStructInput $ \(_, st) -> do
        -- Verify base generic struct exists
        case M.lookup "Box" (structNames st) of
          Nothing -> expectationFailure "Base struct not found"
          Just sid -> do
            let baseDef = lookupStruct sid st
            baseDef `shouldNotBe` Nothing

        -- Verify specialized version exists
        case M.lookup "Box_i32" (structNames st) of
          Nothing -> expectationFailure "Specialized struct not found"
          Just sid -> do
            let specDef = lookupStruct sid st
            specDef `shouldNotBe` Nothing
            case specDef of
              Just def -> do
                structParams def `shouldBe` [] -- No type params in specialized version
                structFields def `shouldBe` [("value", TypeNum Int32)] -- T replaced with i32
              Nothing -> expectationFailure "Could not lookup specialized struct"

    it "substitutes type parameters in struct fields" $ do
      let input =
            T.unlines
              [ "type Box[T] = struct",
                "  value: T",
                "",
                "let b = Box[i32](42)",
                "let x: i32 = b.value"
              ]
      expectParseWithSymbols input $ \(tops, st) -> do
        -- First verify the struct definition
        case tops of
          [TLType name (TypeStruct sid _), _, _] -> do
            name `shouldBe` "Box"
            case lookupStruct sid st of
              Just def -> do
                structName def `shouldBe` "Box"
                structParams def `shouldBe` [TypeParam "T"]
                structFields def `shouldBe` [("value", TypeParam "T")]
              Nothing -> expectationFailure "Base struct not found"
          _ -> expectationFailure $ "Unexpected top level structure: " ++ show tops

        -- Now verify the specialized version exists in symbol table
        let instantiatedSid = case tops of
              [_, TLExpr (Let _ (Call "Box_i32" _)), _] ->
                -- Updated pattern
                case M.lookup "Box_i32" (structNames st) of
                  Just sid -> sid
                  Nothing -> error "Instantiated struct not found"
              other -> error $ "Expected Box instantiation, got: " ++ show other

        -- Verify specialized struct has concrete types
        case lookupStruct instantiatedSid st of
          Just def -> do
            structName def `shouldBe` "Box_i32"
            structParams def `shouldBe` [] -- No type params in concrete version
            structFields def `shouldBe` [("value", TypeNum Int32)] -- T replaced with i32
          Nothing -> expectationFailure "Instantiated struct not found in symbol table"

        -- Verify field access expression has correct type
        case tops of
          [_, _, TLExpr (Let "x" (FieldAccess (Var "b") "value"))] -> return ()
          _ -> expectationFailure "Expected field access with correct variable names"

    it "creates distinct specialized types for different type params" $ do
      let input =
            T.unlines
              [ "type Pair[S, T] = struct",
                "  first: S",
                "  second: T",
                "let p1 = Pair[i64, i32](42'i64, 17'i32)",
                "let p2 = Pair[i32, i64](17'i32, 42'i64)" -- Same types, different order
              ]
      expectParseWithSymbols input $ \(_, st) -> do
        -- Verify both specialized versions exist with correct fields
        let p1Name = "Pair_i64_i32"
        let p2Name = "Pair_i32_i64"

        -- Check first specialization
        case M.lookup p1Name (structNames st) of
          Just sid1 -> case lookupStruct sid1 st of
            Just def1 ->
              structFields def1
                `shouldBe` [("first", TypeNum Int64), ("second", TypeNum Int32)]
            Nothing -> expectationFailure "First specialized struct not found"
          Nothing -> expectationFailure "First specialization not registered"

        -- Check second specialization
        case M.lookup p2Name (structNames st) of
          Just sid2 -> case lookupStruct sid2 st of
            Just def2 ->
              structFields def2
                `shouldBe` [("first", TypeNum Int32), ("second", TypeNum Int64)]
            Nothing -> expectationFailure "Second specialized struct not found"
          Nothing -> expectationFailure "Second specialization not registered"

    it "handles nested type parameter substitution" $ do
      let input =
            T.unlines
              [ "type",
                "  Nested[T] = struct",
                "    inner: Box[T]", -- Nested use of type parameter
                "    value: T",
                "",
                "  Box[S] = struct",
                "    data: S",
                "",
                "let x = Nested[i32]",
                "  (Box[i32](42), 17)", -- Create nested structure
                "",
                "print x.inner.data" -- Should access inner Box's data
              ]
      expectParseWithSymbols input $ \(_, st) -> do
        -- Verify base structs exist
        traceM $ "\n=== Looking up Box struct ==="
        case M.lookup "Box" (structNames st) of
          Nothing -> expectationFailure "Base Box struct not found"
          Just sid -> do
            let baseDef = lookupStruct sid st
            baseDef `shouldNotBe` Nothing

        traceM $ "\n=== Looking up Nested struct ==="
        case M.lookup "Nested" (structNames st) of
          Nothing -> expectationFailure "Base Nested struct not found"
          Just sid -> do
            let baseDef = lookupStruct sid st
            baseDef `shouldNotBe` Nothing

        -- Verify specialized versions exist with substituted types
        traceM $ "\n=== Looking up Box_i32 struct ==="
        case M.lookup "Box_i32" (structNames st) of
          Nothing -> expectationFailure "Specialized Box not found"
          Just sid -> case lookupStruct sid st of
            Just def ->
              structFields def
                `shouldBe` [("data", TypeNum Int32)]
            Nothing -> expectationFailure "Could not lookup specialized Box"

        traceM $ "\n=== Looking up Nested_i32 struct ==="
        case M.lookup "Nested_i32" (structNames st) of
          Nothing -> expectationFailure "Specialized Nested not found"
          Just sid -> case lookupStruct sid st of
            Just def -> do
              -- First lookup Box_i32's ID
              case M.lookup "Box_i32" (structNames st) of
                Nothing -> expectationFailure "Box_i32 not found when checking Nested_i32 fields"
                Just boxSid ->
                  structFields def
                    `shouldBe` [ ("inner", TypeStruct boxSid "Box_i32"),
                                 ("value", TypeNum Int32)
                               ]
            Nothing -> expectationFailure "Could not lookup specialized Nested"

  describe "Field access type tracking" $ do
    it "tracks variable types through field access" $ do
      let input =
            T.unlines
              [ "type Box[T] = struct",
                "  value: T",
                "",
                "let b = Box[i32](42)",
                "let x: i32 = b.value" -- Should know b's type
              ]
      expectParseWithSymbols input $ \(tops, st) -> do
        case tops of
          [_, TLExpr (Let "b" _), _] ->
            lookupVarType "b" st `shouldBe` Just (TypeStruct (StructId 1) "Box_i32")
          _ -> expectationFailure "Expected Box instantiation"

  describe "Multiple type declarations" $ do
    it "parses multiple indented type declarations after type keyword" $ do
      let input =
            T.unlines
              [ "type",
                "  Box[S] = struct",
                "    data: S",
                "",
                "  Nested[S, T] = struct",
                "    inner: Box[T]",
                "    second: S"
              ]
      expectParseWithSymbols input $ \(tops, st) -> do
        case tops of
          [first@(TLType "Box" _), second@(TLType "Nested" _)] -> do
            -- Verify Box struct
            case first of
              TLType _ (TypeStruct sid1 _) -> do
                case lookupStruct sid1 st of
                  Just def -> do
                    structParams def `shouldBe` [TypeParam "S"]
                    structFields def `shouldBe` [("data", TypeParam "S")]
                  Nothing -> expectationFailure "Box struct not found"
              TLType _ typ ->
                expectationFailure $ "Expected Box to be a struct type, got: " ++ show typ

            case second of
              TLType _ (TypeStruct sid2 _) -> do
                case lookupStruct sid2 st of
                  Just def -> do
                    structParams def `shouldBe` [TypeParam "S", TypeParam "T"]
                    structFields def
                      `shouldBe` [ ("inner", TypeStruct (StructId 0) "Box"),
                                   ("second", TypeParam "S")
                                 ]
                  Nothing -> expectationFailure "Nested struct not found"
              TLType _ typ ->
                expectationFailure $ "Expected Nested to be a struct type, got: " ++ show typ
          _ -> expectationFailure $ "Expected two type declarations, got: " ++ show tops
