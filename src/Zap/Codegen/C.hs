{-# LANGUAGE OverloadedStrings #-}
module Zap.Codegen.C
  ( generateC
  , CGenError(..)
  ) where

import Data.Char (isSpace)
import qualified Data.List as L
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Debug.Trace

import Zap.IR
import Zap.AST (Op(..), StructId(..), StructDef(..), SymbolTable(..), Type(..), lookupStruct)

data CGenError
  = UnsupportedType IRType
  | UnsupportedOperation T.Text
  deriving (Show, Eq)

generateC :: IRProgram -> Either CGenError T.Text
generateC (IRProgram funcs) = do
    traceM "\n=== Starting C Code Generation ==="

    -- Generate struct definitions
    let structDefs = generateStructDefinitions funcs

    -- Generate each function and combine results
    functionDefs <- mapM generateFunction funcs

    let program = T.unlines
          [ "#include <stdio.h>"
          , "#include <stdint.h>"
          , ""
          , structDefs  -- Add struct definitions
          , T.unlines functionDefs
          ]

    traceM $ "\n=== Final Generated Program ===\n" ++ T.unpack program
    return program

-- Helper to generate struct definitions
generateStructDefinitions :: [(IRFuncDecl, IRMetadata)] -> T.Text
generateStructDefinitions funcs =
    let structTypes = collectStructTypes funcs
    in T.unlines $ map genStructDef structTypes
  where
    genStructDef :: (String, [(String, IRType)]) -> T.Text
    genStructDef (name, fields) = T.unlines $ map rstrip $ map T.unpack $
        [ T.concat ["struct ", T.pack name, " {"]
        , T.intercalate ";\n" $ map genField fields ++ [""]  -- Add trailing semicolon
        , "};"
        , ""  -- Add blank line between struct definitions
        ]

    genField :: (String, IRType) -> T.Text
    genField (fname, ftype) = T.concat
        ["    ", irTypeToC ftype, " ", T.pack fname]

collectStructTypes :: [(IRFuncDecl, IRMetadata)] -> [(String, [(String, IRType)])]
collectStructTypes funcs =
    let mainStructs = case funcs of
          [(mainFn, meta)] -> do
              traceM $ "\n=== Collecting struct types ==="
              traceM $ "Metadata symbol table: " ++ show (metaSymTable meta)
              findStructTypes (fnBody mainFn) (metaSymTable meta)
          _ -> []
    in L.nubBy (\(n1,_) (n2,_) -> n1 == n2) mainStructs
  where
    findStructTypes :: IRBlock -> Maybe SymbolTable -> [(String, [(String, IRType)])]
    findStructTypes (IRBlock _ stmts) symTable =
      concatMap (findStmtStructTypes symTable) stmts

    findStmtStructTypes :: Maybe SymbolTable -> (IRStmt, IRMetadata) -> [(String, [(String, IRType)])]
    findStmtStructTypes symTable (stmt, _) = do
        traceM $ "\n=== Finding struct types in statement ==="
        traceM $ "Statement: " ++ show stmt
        traceM $ "Symbol table: " ++ show symTable
        case stmt of
          IRVarDecl _ (IRTypeStruct name sid) init ->
            case init of
              IRCall "struct_lit" (IRLit (IRStringLit structName):args) ->
                if isGenericStructName structName
                  then [(structName, [("value", getArgType (head args))])]
                  else case symTable >>= lookupStruct sid of
                         Just def ->
                           let fields = structFields def
                           in traceM ("Found struct fields: " ++ show fields) >>
                              [(structName,
                                zipWith (\(fname, _) arg -> (fname, getArgType arg))
                                       fields args)]
                         Nothing -> traceM "No struct definition found" >> []
              _ -> []
          _ -> []

    isGenericStructName :: String -> Bool
    isGenericStructName name = any (\suffix -> suffix `L.isSuffixOf` name)
                                  ["_i32", "_i64", "_f32", "_f64"]

    getArgType :: IRExpr -> IRType
    getArgType (IRLit (IRInt32Lit _)) = IRTypeInt32
    getArgType (IRLit (IRInt64Lit _)) = IRTypeInt64
    getArgType (IRLit (IRFloat32Lit _)) = IRTypeFloat32
    getArgType (IRLit (IRFloat64Lit _)) = IRTypeFloat64
    getArgType _ = IRTypeInt32

-- Helper to convert IR types to C types
irTypeToC :: IRType -> T.Text
irTypeToC IRTypeInt32 = "int32_t"
irTypeToC IRTypeInt64 = "int64_t"
irTypeToC IRTypeFloat32 = "float"
irTypeToC IRTypeFloat64 = "double"
irTypeToC (IRTypeStruct name _) = T.concat ["struct ", T.pack name]
irTypeToC _ = "void"  -- Default case

generateFunction :: (IRFuncDecl, IRMetadata) -> Either CGenError T.Text
generateFunction (func, _) = do
    body <- generateBlock (fnBody func)

    -- Determine return type for function signature
    let typeStr = case fnRetType func of
          IRTypeInt32 -> "int32_t"
          IRTypeVoid -> "void"
          _ -> "int"  -- Default to int for now

    -- Generate function signature based on name and parameters
    let signature = case fnName func of
          "main" -> T.pack "int main(void)"  -- Preserve existing main() handling
          _ -> T.concat [T.pack typeStr, " ", T.pack (fnName func), "(", generateParams (fnParams func), ")"]

    return $ T.unlines $ map rstrip $ map T.unpack $
      [ signature <> T.pack " {"
      , body
      , "}"
      ]

  where
    -- Helper to generate parameter list
    generateParams :: [(String, IRType)] -> T.Text
    generateParams [] = T.pack "void"
    generateParams params = T.intercalate ", " $ map formatParam params

    -- Helper to format individual parameters
    formatParam :: (String, IRType) -> T.Text
    formatParam (name, typ) = T.concat [paramTypeToC typ, " ", T.pack name]

    -- Helper to convert IR types to C types
    paramTypeToC :: IRType -> T.Text
    paramTypeToC IRTypeInt32 = "int32_t"
    paramTypeToC IRTypeVoid = "void"
    paramTypeToC _ = "int"  -- Default to int for now

generateBlock :: IRBlock -> Either CGenError T.Text
generateBlock (IRBlock _ stmts) = do
    generatedStmts <- mapM generateStmt stmts
    return $ T.unlines generatedStmts

generateStmt :: (IRStmt, IRMetadata) -> Either CGenError T.Text
generateStmt (stmt, _) = case stmt of
    IRVarDecl name irType initExpr -> do
        exprCode <- generateExpr initExpr
        -- Use proper type for struct declarations
        let typeStr = case irType of
              IRTypeStruct sname _ -> T.concat ["struct ", T.pack sname]
              _ -> irTypeToC irType
        return $ T.concat ["    ", typeStr, " ", T.pack name, " = ", exprCode, ";"]
    IRStmtExpr expr -> do
        exprCode <- generateExpr expr
        return $ T.concat ["    ", exprCode, ";"]  -- Add semicolon here
    IRReturn Nothing -> return "    return 0;"
    IRReturn (Just expr) -> do
        exprCode <- generateExpr expr
        return $ T.concat ["    return ", exprCode, ";"]
    IRAssign name expr -> do
        exprCode <- generateExpr expr
        return $ T.concat ["    ", T.pack name, " = ", exprCode, ";"]
    IRAssignOp name op expr -> do
        exprCode <- generateExpr expr
        let opStr = case op of
              Add -> "+="
              Sub -> "-="
              Mul -> "*="
              Div -> "/="
              _ -> error $ "Unsupported compound assignment operator: " ++ show op
        return $ T.concat ["    ", T.pack name, " ", T.pack opStr, " ", exprCode, ";"]
    IRLabel label ->
        return $ T.concat [T.pack label, ":"]
    IRGoto label ->
        return $ T.concat ["    goto ", T.pack label, ";"]
    IRJumpIfTrue cond label -> do
        condCode <- generateExpr cond
        return $ T.concat ["    if (", condCode, ") goto ", T.pack label, ";"]
    IRJumpIfZero cond label -> do
        condCode <- generateExpr cond
        return $ T.concat ["    if (!(", condCode, ")) goto ", T.pack label, ";"]
    IRProcCall "print" [expr] -> do
        (value, fmt) <- generatePrintExpr expr
        return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]
    IRProcCall name _ ->
        Left $ UnsupportedOperation $ T.pack $ "Unsupported procedure: " ++ name

-- Helper to detect if a condition is already negated
isNegatedCondition :: T.Text -> Bool
isNegatedCondition cond =
    "!(" `T.isPrefixOf` cond && ")" `T.isSuffixOf` cond

-- Helper to remove negation from a condition
unnegateCondition :: T.Text -> T.Text
unnegateCondition cond =
    let inner = T.drop 2 $ T.dropEnd 1 cond -- Remove "!(" and ")"
    in inner

-- Helper for generating literal values
generateLiteral :: IRLiteral -> T.Text
generateLiteral (IRInt32Lit n) = T.pack $ show n
generateLiteral (IRInt64Lit n) = T.pack $ show n ++ "L"  -- Use C long suffix
generateLiteral (IRFloat32Lit n) = T.pack (show n ++ "f")  -- Use C float suffix
generateLiteral (IRFloat64Lit n) = T.pack $ show n
generateLiteral (IRVarRef name) = T.pack name
generateLiteral (IRStringLit s) = T.concat ["\"", T.pack s, "\""]

generateExpr :: IRExpr -> Either CGenError T.Text
generateExpr expr = do
    traceM $ "\n=== generateExpr called with: " ++ show expr
    case expr of
        IRCall "field_access" [baseExpr, IRLit (IRStringLit field)] -> do
             traceM $ "\n=== generateExpr: field_access ==="
             traceM $ "Base expr: " ++ show baseExpr
             traceM $ "Field: " ++ field
             base <- generateExpr baseExpr
             let result = T.concat [base, ".", T.pack field]
             traceM $ "Generated field access: " ++ T.unpack result
             return result

        IRCall "struct_lit" (IRLit (IRStringLit name):fields) -> do
            traceM $ "Generating struct literal"
            traceM $ "Struct name: " ++ name
            traceM $ "Fields: " ++ show fields
            fieldVals <- mapM generateExpr fields
            let result = T.concat ["(struct ", T.pack name, ") {", T.intercalate ", " fieldVals, "}"]
            traceM $ "Generated struct init: " ++ T.unpack result
            return result

        IRCall "print" [expr] -> do
            traceM $ "\n=== generateExpr: print ==="
            traceM $ "Print argument: " ++ show expr
            (value, fmt) <- generatePrintExpr expr
            traceM $ "Generated print: value=" ++ T.unpack value ++ ", fmt=" ++ T.unpack fmt
            return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]

        IRCall op [e1, e2] -> do
            traceM $ "\n=== generateExpr: binary op ==="
            traceM $ "Operator: " ++ op
            traceM $ "Left expr: " ++ show e1
            traceM $ "Right expr: " ++ show e2
            -- Map operator strings to C operators
            left <- generateExpr e1
            right <- generateExpr e2
            let cOp = case op of
                  "Lt" -> "<"
                  "Gt" -> ">"
                  "Eq" -> "=="
                  "NotEq" -> "!="
                  "Add" -> "+"
                  "Sub" -> "-"
                  "Mul" -> "*"
                  "Div" -> "/"
                  "Point" -> error $ "Found struct constructor in binary op path" -- Added debug case
                  _ -> error $ "Unsupported operator: " ++ op
            return $ T.concat ["(", left, ") ", T.pack cOp, " (", right, ")"]

        IRLit lit -> return $ generateLiteral lit

        IRVar name -> return $ T.pack name

        expr -> Left $ UnsupportedOperation $ T.pack $ "Unsupported expression: " ++ show expr

-- Helper to determine format specifier
generatePrintExpr :: IRExpr -> Either CGenError (T.Text, T.Text)
generatePrintExpr (IRCall "field_access" [baseExpr, IRLit (IRStringLit field)]) = do
    base <- generateExpr baseExpr
    return (T.concat [base, ".", T.pack field], "%d")
generatePrintExpr (IRLit (IRStringLit str)) =
    Right (T.concat ["\"", T.pack str, "\""], "%s")
generatePrintExpr (IRLit (IRInt32Lit n)) =
    Right (T.pack (show n), "%d")
generatePrintExpr (IRLit (IRInt64Lit n)) =
    Right (T.pack (show n), "%lld")
generatePrintExpr (IRLit (IRFloat32Lit n)) =
    Right (T.pack (show n), "%f")
generatePrintExpr (IRLit (IRFloat64Lit n)) =
    Right (T.pack (show n), "%lf")
generatePrintExpr (IRVar name) =
    Right (T.pack name, "%d")  -- Default to int format
generatePrintExpr (IRLit (IRVarRef name)) =
    Right (T.pack name, "%d")  -- Assuming variables are ints for now
generatePrintExpr (IRCall op [e1, e2]) = case op of
    "Lt" -> do
        left <- generateExpr e1
        right <- generateExpr e2
        Right (T.concat ["((", left, ") < (", right, ") ? 1 : 0)"], "%d")
    "Gt" -> do
        left <- generateExpr e1
        right <- generateExpr e2
        Right (T.concat ["((", left, ") > (", right, ") ? 1 : 0)"], "%d")
    "Eq" -> do
        left <- generateExpr e1
        right <- generateExpr e2
        Right (T.concat ["((", left, ") == (", right, ") ? 1 : 0)"], "%d")
    "NotEq" -> do
        left <- generateExpr e1
        right <- generateExpr e2
        Right (T.concat ["((", left, ") != (", right, ") ? 1 : 0)"], "%d")
    _ -> do
        -- For other function calls, fall back to default behavior
        argCode <- mapM generateExpr [e1, e2]
        let call = T.concat [T.pack op, "(", T.intercalate ", " argCode, ")"]
        Right (call, "%d")
generatePrintExpr expr =
    -- Handle other expressions by generating them normally
    generateExpr expr >>= \exprCode -> Right (exprCode, "%d")

rstrip = T.pack . reverse . dropWhile isSpace . reverse
