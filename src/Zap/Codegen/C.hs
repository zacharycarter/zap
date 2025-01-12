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

data CGState = CGState {
  cgVarTypes :: M.Map String IRType
}

initialCGState :: CGState
initialCGState = CGState M.empty

data CGenError
  = UnsupportedType IRType
  | UnsupportedOperation T.Text
  deriving (Show, Eq)

generateC :: IRProgram -> Either CGenError T.Text
generateC prog = evalStateT (generateCWithState prog) (CGState M.empty)

generateCWithState :: IRProgram -> StateT CGState (Either CGenError) T.Text
generateCWithState (IRProgram funcs) = do
    lift $ traceM "\n=== Starting C Code Generation ==="

    -- Generate struct definitions (no state needed)
    let structDefs = generateStructDefinitions funcs

    -- Generate each function with state
    functionDefs <- mapM generateFunctionWithState funcs

    let program = T.unlines
          [ "#include <stdio.h>"
          , "#include <stdint.h>"
          , ""
          , structDefs
          , T.unlines functionDefs
          ]

    lift $ traceM $ "\n=== Final Generated Program ===\n" ++ T.unpack program
    return program

-- Helper to generate struct definitions
generateStructDefinitions :: [(IRFuncDecl, IRMetadata)] -> T.Text
generateStructDefinitions funcs =
    let structTypes = collectStructTypes funcs
    in T.unlines $ map genStructDef structTypes
  where
    genStructDef :: (String, [(String, IRType)]) -> T.Text
    genStructDef (name, fields) = T.unlines $ map rstrip $ map T.unpack $
        [ T.concat ["struct ", T.pack name, " {"]  -- Use specialized name throughout
        , T.intercalate ";\n" $ map genField fields ++ [""]
        , "};"
        , ""
        ]
    -- genStructDef :: (String, [(String, IRType)]) -> T.Text
    -- genStructDef (name, fields) = T.unlines $ map rstrip $ map T.unpack $
    --     [ T.concat ["struct ", T.pack name, " {"]
    --     , T.intercalate ";\n" $ map genField fields ++ [""]  -- Add trailing semicolon
    --     , "};"
    --     , ""  -- Add blank line between struct definitions
    --     ]

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
              IRCall "struct_lit" _ -> -- Use specialized name from IRTypeStruct
                case symTable >>= lookupStruct sid of
                  Just def ->
                    let fields = structFields def
                        fieldTypes = zipWith (\(fname, _) arg -> (fname, getArgType arg))
                                           fields
                                           (drop 1 $ case init of IRCall _ args -> args; _ -> [])
                    in [(name, fieldTypes)]  -- Use the specialized name here
                  Nothing -> []
              -- IRCall "struct_lit" (IRLit (IRStringLit structName):args) ->
              --   if isGenericStructName structName
              --     then [(structName, [("value", getArgType (head args))])]
              --     else case symTable >>= lookupStruct sid of
              --            Just def ->
              --              let fields = structFields def
              --              in traceM ("Found struct fields: " ++ show fields) >>
              --                 [(structName,
              --                   zipWith (\(fname, _) arg -> (fname, getArgType arg))
              --                          fields args)]
              --            Nothing -> traceM "No struct definition found" >> []
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

generateFunctionWithState :: (IRFuncDecl, IRMetadata) -> StateT CGState (Either CGenError) T.Text
generateFunctionWithState (func, _) = do
    -- Convert body with state tracking
    body <- generateBlockWithState (fnBody func)

    -- Determine return type (unchanged)
    let typeStr = case fnRetType func of
          IRTypeInt32 -> "int32_t"
          IRTypeVoid -> "void"
          _ -> "int"  -- Default to int for now

    -- Generate function signature (unchanged)
    let signature = case fnName func of
          "main" -> T.pack "int main(void)"
          _ -> T.concat [T.pack typeStr, " ", T.pack (fnName func), "(", generateParams (fnParams func), ")"]

    -- Return function text (unchanged but lift the string creation)
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

generateBlockWithState :: IRBlock -> StateT CGState (Either CGenError) T.Text
generateBlockWithState (IRBlock _ stmts) = do
  -- Convert statements with state tracking
  generatedStmts <- mapM generateStmtWithState stmts
  return $ T.unlines generatedStmts

generateStmtWithState :: (IRStmt, IRMetadata) -> StateT CGState (Either CGenError) T.Text
generateStmtWithState (stmt, _) = case stmt of
    IRVarDecl name irType@(IRTypeStruct structName _) initExpr -> do
        -- Track both the temporary struct type for initialization and the variable type
        modify $ \s -> s { cgVarTypes = M.insert "current_struct_type" irType $
                                     M.insert name irType $
                                     cgVarTypes s }
        exprCode <- generateExprWithState initExpr
        -- Only remove the temporary struct type, keep the variable type
        modify $ \s -> s { cgVarTypes = M.delete "current_struct_type" (cgVarTypes s) }
        return $ T.concat ["    struct ", T.pack structName, " ", T.pack name, " = ", exprCode, ";"]

    IRVarDecl name irType initExpr -> do
        -- This case remains unchanged as it already tracks variable types correctly
        modify $ \s -> s { cgVarTypes = M.insert name irType (cgVarTypes s) }
        exprCode <- generateExprWithState initExpr
        let typeStr = case irType of
              IRTypeStruct sname _ -> T.concat ["struct ", T.pack sname]
              _ -> irTypeToC irType
        return $ T.concat ["    ", typeStr, " ", T.pack name, " = ", exprCode, ";"]

    IRStmtExpr expr -> do
        exprCode <- generateExprWithState expr
        return $ T.concat ["    ", exprCode, ";"]

    IRReturn Nothing ->
        return "    return 0;"

    IRReturn (Just expr) -> do
        exprCode <- generateExprWithState expr
        return $ T.concat ["    return ", exprCode, ";"]

    IRAssign name expr -> do
        exprCode <- generateExprWithState expr
        return $ T.concat ["    ", T.pack name, " = ", exprCode, ";"]

    IRAssignOp name op expr -> do
        exprCode <- generateExprWithState expr
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
        condCode <- generateExprWithState cond
        return $ T.concat ["    if (", condCode, ") goto ", T.pack label, ";"]

    IRJumpIfZero cond label -> do
        condCode <- generateExprWithState cond
        return $ T.concat ["    if (!(", condCode, ")) goto ", T.pack label, ";"]

    IRProcCall "print" [expr] -> do
        (value, fmt) <- generatePrintExprWithState expr
        return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]

    IRProcCall name _ ->
        lift $ Left $ UnsupportedOperation $ T.pack $ "Unsupported procedure: " ++ name

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

generateExprWithState :: IRExpr -> StateT CGState (Either CGenError) T.Text
generateExprWithState expr = do
    traceM $ "\n=== generateExpr called with: " ++ show expr
    case expr of
        IRCall "field_access" [baseExpr, IRLit (IRStringLit field)] -> do
            traceM $ "\n=== generateExpr: field_access ==="
            base <- generateExprWithState baseExpr
            let result = T.concat [base, ".", T.pack field]
            traceM $ "Generated field access: " ++ T.unpack result
            return result

        IRCall "struct_lit" (IRLit (IRStringLit name):fields) -> do
            traceM "Generating struct literal"
            traceM $ "Struct name: " ++ name
            fieldVals <- mapM generateExprWithState fields

            -- Get target struct type from variable declaration context
            st <- get
            let structName = case M.lookup "current_struct_type" (cgVarTypes st) of
                             Just (IRTypeStruct specializedName _) -> specializedName
                             _ -> name

            let result = T.concat ["(struct ", T.pack structName, ") {", T.intercalate ", " fieldVals, "}"]
            traceM $ "Generated struct init: " ++ T.unpack result
            return result

        IRCall "print" [expr] -> do
            traceM $ "\n=== generateExpr: print ==="
            traceM $ "Print argument: " ++ show expr
            (value, fmt) <- generatePrintExprWithState expr
            traceM $ "Generated print: value=" ++ T.unpack value ++ ", fmt=" ++ T.unpack fmt
            return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]

        IRCall op [e1, e2] -> do
            traceM $ "\n=== generateExpr: binary op ==="
            traceM $ "Operator: " ++ op
            traceM $ "Left expr: " ++ show e1
            traceM $ "Right expr: " ++ show e2
            -- Map operator strings to C operators
            left <- generateExprWithState e1
            right <- generateExprWithState e2
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

        expr -> lift $ Left $ UnsupportedOperation $ T.pack $ "Unsupported expression: " ++ show expr

-- Helper to determine format specifier
generatePrintExprWithState :: IRExpr -> StateT CGState (Either CGenError) (T.Text, T.Text)
generatePrintExprWithState expr = do
    traceM $ "\n=== generatePrintExpr ==="
    traceM $ "Input expression: " ++ show expr
    case expr of
        IRCall "field_access" [baseExpr, IRLit (IRStringLit field)] -> do
            traceM $ "Processing field access:"
            traceM $ "  Base expr: " ++ show baseExpr
            traceM $ "  Field: " ++ field
            base <- generateExprWithState baseExpr
            traceM $ "  Generated base: " ++ T.unpack base

            -- Get stored type information for the variable
            st <- get
            let fmt = case baseExpr of
                  IRVar name -> case M.lookup name (cgVarTypes st) of
                    Just (IRTypeStruct sname _) ->
                        if "_i64" `T.isSuffixOf` T.pack sname then "%ld"
                        else if "_f32" `T.isSuffixOf` T.pack sname then "%f"
                        else if "_f64" `T.isSuffixOf` T.pack sname then "%lf"
                        else "%d"
                    _ -> "%d"
                  _ -> "%d"

            traceM $ "  Selected format: " ++ T.unpack fmt
            return (T.concat [base, ".", T.pack field], fmt)

        IRLit (IRStringLit str) ->
            return (T.concat ["\"", T.pack str, "\""], "%s")

        IRLit (IRInt32Lit n) ->
            return (T.pack (show n), "%d")

        IRLit (IRInt64Lit n) ->
            return (T.pack (show n ++ "L"), "%ld")

        IRLit (IRFloat32Lit n) ->
            return (T.pack (show n), "%f")

        IRLit (IRFloat64Lit n) ->
            return (T.pack (show n), "%lf")

        IRVar name -> do
            st <- get
            let fmt = case M.lookup name (cgVarTypes st) of
                  Just IRTypeInt32 -> "%d"
                  Just IRTypeInt64 -> "%ld"
                  Just IRTypeFloat32 -> "%f"
                  Just IRTypeFloat64 -> "%lf"
                  _ -> "%d"  -- Fallback
            traceM $ "Variable " ++ name ++ " type format: " ++ T.unpack fmt
            return (T.pack name, fmt)

        IRLit (IRVarRef name) ->
            return (T.pack name, "%d")

        IRCall op [e1, e2] -> do
            left <- generateExprWithState e1
            right <- generateExprWithState e2
            case op of
                "Lt" -> return (T.concat ["((", left, ") < (", right, ") ? 1 : 0)"], "%d")
                "Gt" -> return (T.concat ["((", left, ") > (", right, ") ? 1 : 0)"], "%d")
                "Eq" -> return (T.concat ["((", left, ") == (", right, ") ? 1 : 0)"], "%d")
                "NotEq" -> return (T.concat ["((", left, ") != (", right, ") ? 1 : 0)"], "%d")
                _ -> do
                    argCode <- mapM generateExprWithState [e1, e2]
                    let call = T.concat [T.pack op, "(", T.intercalate ", " argCode, ")"]
                    return (call, "%d")

        expr -> do
            exprCode <- generateExprWithState expr
            return (exprCode, "%d")

rstrip = T.pack . reverse . dropWhile isSpace . reverse
