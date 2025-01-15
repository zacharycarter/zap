{-# LANGUAGE OverloadedStrings #-}
module Zap.Codegen.C
  ( generateC
  , CGenError(..)
  ) where

import Data.Char (isSpace)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad (forM_, when)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Debug.Trace

import Zap.IR
import Zap.AST

data CGState = CGState
  { cgVarTypes :: M.Map String IRType
  , cgUsedLabels :: S.Set String
  , cgDeclaredLabels :: S.Set String
  }

initialCGState :: CGState
initialCGState = CGState
  { cgVarTypes = M.empty
  , cgUsedLabels = S.empty
  , cgDeclaredLabels = S.empty
  }

data CGenError
  = UnsupportedType IRType
  | UnsupportedOperation T.Text
  deriving (Show, Eq)

generateC :: IRProgram -> Either CGenError T.Text
generateC prog = evalStateT (generateCWithState prog) (CGState M.empty S.empty S.empty)

generateCWithState :: IRProgram -> StateT CGState (Either CGenError) T.Text
generateCWithState (IRProgram funcs) = do
    lift $ traceM "\n=== Starting C Code Generation ==="

    -- Generate struct definitions (keep existing implementation)
    let structDefs = generateStructDefinitions funcs

    -- Add new: Generate function declarations
    let declarations = map generateFunctionDeclaration $ filter (not . isMainFunc . fst) funcs

    lift $ traceM $ "\nFunctions to generate: " ++ show (map (fnName . fst) funcs)
    -- Generate each function with state
    functionDefs <- mapM generateFunctionWithState funcs

    let program = T.unlines
          [ "#include <stdio.h>"
          , "#include <stdint.h>"
          , ""
          , structDefs
          , ""
          , T.unlines declarations  -- Add function declarations
          , ""
          , T.unlines functionDefs
          ]

    lift $ traceM $ "\n=== Final Generated Program ===\n" ++ T.unpack program

    checkUnusedLabels

    return program
  where
    isMainFunc (IRFuncDecl {fnName = "main"}) = True
    isMainFunc _ = False

-- Debug helper for checking unused labels
checkUnusedLabels :: StateT CGState (Either CGenError) ()
checkUnusedLabels = do
    st <- get
    let declared = cgDeclaredLabels st
    let used = cgUsedLabels st
    let unused = S.difference declared used
    when (not $ S.null unused) $
        lift $ traceM $ "Warning: Unused labels: " ++ show (S.toList unused)

-- Helper for generating function declarations
generateFunctionDeclaration :: (IRFuncDecl, IRMetadata) -> T.Text
generateFunctionDeclaration (func, _) =
    let typeStr = irTypeToC (fnRetType func)
        params = T.intercalate ", " $ map (\(name, typ) ->
                  T.concat [irTypeToC typ, " ", T.pack name]) (fnParams func)
    in T.concat [typeStr, " ", T.pack (fnName func), "(", params, ");"]

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

    genField :: (String, IRType) -> T.Text
    genField (fname, ftype) =
        let
            _ = trace ("\n=== generateField ===\n" ++
                      "Field: " ++ fname ++ "\n" ++
                      "Type: " ++ show ftype) ()
        in case ftype of
            IRTypeStruct sname _ ->
                let
                    _ = trace ("Generating struct field with name: " ++ sname) ()
                in T.concat ["    struct ", T.pack sname, " ", T.pack fname]
            _ -> T.concat ["    ", irTypeToC ftype, " ", T.pack fname]

collectStructTypes :: [(IRFuncDecl, IRMetadata)] -> [(String, [(String, IRType)])]
collectStructTypes funcs =
    let allStructs = case funcs of
          [(mainFn, meta)] -> do
              traceM $ "\n=== collectStructTypes: initial collection ==="
              traceM $ "Metadata symbol table: "
              case metaSymTable meta of
                Just st -> do
                    traceM $ "  Struct definitions:"
                    forM_ (M.toList $ structDefs st) $ \(sid, def) ->
                        traceM $ "    " ++ show sid ++ " -> " ++ show def
                    traceM $ "  Struct names:"
                    forM_ (M.toList $ structNames st) $ \(name, sid) ->
                        traceM $ "    " ++ name ++ " -> " ++ show sid
                Nothing -> traceM "  No symbol table found"

              let found = findStructTypes (fnBody mainFn) (metaSymTable meta)
              traceM $ "\nFound direct structs and their fields:"
              forM_ found $ \(name, fields) -> do
                  traceM $ "  Struct: " ++ name
                  forM_ fields $ \(fname, ftype) ->
                      traceM $ "    Field: " ++ fname ++ " -> " ++ show ftype
              found
          _ -> []

    in do
        traceM $ "\n=== collectStructTypes: dependency ordering ==="
        let deps = [(name, getStructDeps fields) | (name, fields) <- allStructs]
        traceM "Dependencies:"
        forM_ deps $ \(name, depList) ->
            traceM $ "  " ++ name ++ " depends on: " ++ show depList

        let ordered = orderStructsByDeps allStructs
        traceM "\nFinal ordered structs:"
        forM_ ordered $ \(name, fields) -> do
            traceM $ "  Struct: " ++ name
            forM_ fields $ \(fname, ftype) ->
                traceM $ "    Field: " ++ fname ++ " -> " ++ show ftype
        ordered
  where
    findStructTypes :: IRBlock -> Maybe SymbolTable -> [(String, [(String, IRType)])]
    findStructTypes (IRBlock _ stmts) symTable =
        let directStructs = concatMap (findStmtStructTypes symTable) stmts
            -- Add dependent structs from fields and process them recursively
            depStructs = concatMap (getDependentStructs symTable) directStructs
        in L.nubBy (\(n1,_) (n2,_) -> n1 == n2) (directStructs ++ depStructs)
      where
        findStmtStructTypes :: Maybe SymbolTable -> (IRStmt, IRMetadata) -> [(String, [(String, IRType)])]
        findStmtStructTypes symTable (stmt, _) = do
            traceM $ "\n=== Finding struct types in statement ==="
            traceM $ "Statement: " ++ show stmt
            traceM $ "Symbol table: " ++ show symTable
            case stmt of
                IRVarDecl _ (IRTypeStruct name sid) init ->
                    case symTable >>= lookupStruct sid of
                        Just def ->
                            -- Register base constructor function
                            [(name, map (convertFieldType symTable) (structFields def))]
                        Nothing -> []
                _ -> []
               
        getDependentStructs :: Maybe SymbolTable -> (String, [(String, IRType)]) -> [(String, [(String, IRType)])]
        getDependentStructs symTable (name, fields) =
            -- Extract each struct field type and get its definition
            [ (depName, getStructFields symTable depSid)
            | (_, fieldType) <- fields  -- First bind all fields
            , case fieldType of  -- Then match struct types
                IRTypeStruct depName depSid   -- Get struct name and ID
                    | depName /= name ->  -- Avoid recursion
                        True  -- Include this struct
                    | otherwise ->
                        False  -- Skip self-references
                _ -> False  -- Skip non-struct fields
            , let IRTypeStruct depName depSid = fieldType  -- Bind the matched values
            ]  -- The result tuple uses the bound depName and depSid

        getStructFields :: Maybe SymbolTable -> StructId -> [(String, IRType)]
        getStructFields (Just st) sid = case lookupStruct sid st of
            Just def -> map (convertFieldType (Just st)) (structFields def)
            Nothing -> []
        getStructFields Nothing _ = []

    convertFieldType :: Maybe SymbolTable -> (String, Type) -> (String, IRType)
    convertFieldType symTable (name, typ) = case typ of
        TypeStruct sid structName ->
            case symTable >>= lookupStruct sid of
                Just def ->
                    case structParams def of
                        [] -> (name, IRTypeStruct structName sid)  -- Not generic
                        _  ->
                            -- For generic structs, look up concrete type
                            case lookupSpecializedStruct symTable structName of
                                Just (specName, specSid) ->
                                    (name, IRTypeStruct specName specSid)
                                Nothing -> (name, IRTypeStruct structName sid)
                Nothing -> (name, IRTypeStruct structName sid)
        TypeNum Int32 -> (name, IRTypeInt32)
        TypeNum Int64 -> (name, IRTypeInt64)
        TypeNum Float32 -> (name, IRTypeFloat32)
        TypeNum Float64 -> (name, IRTypeFloat64)
        TypeParam param ->
            -- Look up concrete type for this parameter
            case symTable >>= lookupConcreteType param of
                Just concrete -> (name, concrete)
                Nothing -> (name, IRTypeInt32)  -- Default, but log warning

-- Helper to find specialized version of a struct
lookupSpecializedStruct :: Maybe SymbolTable -> String -> Maybe (String, StructId)
lookupSpecializedStruct (Just st) baseName =
    let specializations = filter (\(name, _) -> (T.pack baseName) `T.isPrefixOf` (T.pack name))
                         $ M.toList $ structNames st
    in case specializations of
        [(name, sid)] -> Just (name, sid)
        _ -> Nothing
lookupSpecializedStruct Nothing _ = Nothing

-- Helper to find concrete type for a type parameter
lookupConcreteType :: String -> SymbolTable -> Maybe IRType
lookupConcreteType param st =
    -- Look through struct defs directly using Map.foldrWithKey
    let specializationMap = M.foldrWithKey findConcreteTypes M.empty (structDefs st)
    in M.lookup param specializationMap
  where
    findConcreteTypes :: StructId -> StructDef -> M.Map String IRType -> M.Map String IRType
    findConcreteTypes _ def acc =
        case (structParams def, structFields def) of
            ([param'], [(_, TypeNum Int32)]) -> M.insert param' IRTypeInt32 acc
            ([param'], [(_, TypeNum Int64)]) -> M.insert param' IRTypeInt64 acc
            ([param'], [(_, TypeNum Float32)]) -> M.insert param' IRTypeFloat32 acc
            ([param'], [(_, TypeNum Float64)]) -> M.insert param' IRTypeFloat64 acc
            _ -> acc

getStructDeps :: [(String, IRType)] -> [String]
getStructDeps fields = [name | (_, IRTypeStruct name _) <- fields]

orderStructsByDeps :: [(String, [(String, IRType)])] -> [(String, [(String, IRType)])]
orderStructsByDeps structs =
    let -- Build dependency graph: struct name -> list of struct names it depends on
        deps = [(name, getStructDeps fields) | (name, fields) <- structs]

        -- Helper to check if struct A depends on struct B
        dependsOn name1 name2 =
            case lookup name1 deps of
                Just depList -> name2 `elem` depList
                Nothing -> False

        -- Sort based on dependencies - if A depends on B, B comes first
        sortedNames = L.sortBy (\n1 n2 ->
            if dependsOn n1 n2 then GT
            else if dependsOn n2 n1 then LT
            else EQ) (map fst structs)
    in [(name, fields) | name <- sortedNames,
                        (n, fields) <- structs,
                        n == name]

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
    traceM $ "\n=== generateFunctionWithState: " ++ show func ++ " ==="
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
        traceM $ "\n=== generateStmtWithState: IRVarDecl | IRTypeStruct ==="
        -- Track both the temporary struct type for initialization and the variable type
        modify $ \s -> s { cgVarTypes = M.insert "current_struct_type" irType $
                                     M.insert name irType $
                                     cgVarTypes s }
        exprCode <- generateExprWithState initExpr
        -- Only remove the temporary struct type, keep the variable type
        modify $ \s -> s { cgVarTypes = M.delete "current_struct_type" (cgVarTypes s) }
        return $ T.concat ["    struct ", T.pack structName, " ", T.pack name, " = ", exprCode, ";"]

    IRVarDecl name irType initExpr -> do
        traceM $ "\n=== generateStmtWithState: IRVarDecl ==="
        -- This case remains unchanged as it already tracks variable types correctly
        modify $ \s -> s { cgVarTypes = M.insert name irType (cgVarTypes s) }
        exprCode <- generateExprWithState initExpr
        let typeStr = case irType of
              IRTypeStruct sname _ -> T.concat ["struct ", T.pack sname]
              _ -> irTypeToC irType
        return $ T.concat ["    ", typeStr, " ", T.pack name, " = ", exprCode, ";"]

    IRStmtExpr (IRLit (IRBoolLit False)) -> do
        traceM $ "\n=== generateStmtWithState: IRStmtExpr | IRBoolLit False ==="
        -- Don't generate return for break's false literal
        return ""

    IRStmtExpr expr -> do
        traceM $ "\n=== generateStmtWithState: IRStmtExpr ==="
        exprCode <- generateExprWithState expr
        -- return $ T.concat ["    return ", exprCode, ";"]
        return $ T.concat ["    return ", exprCode, ";"]

    IRReturn Nothing -> do
        traceM $ "\n=== generateStmtWithState: IRReturn Nothing ==="
        return "    return 0;"

    IRReturn (Just expr) -> do
        traceM $ "\n=== generateStmtWithState: IRReturn ==="
        exprCode <- generateExprWithState expr
        return $ T.concat ["    return ", exprCode, ";"]

    IRAssign name expr -> do
        traceM $ "\n=== generateStmtWithState: IRAssign ==="
        exprCode <- generateExprWithState expr
        return $ T.concat ["    ", T.pack name, " = ", exprCode, ";"]

    IRAssignOp name op expr -> do
        traceM $ "\n=== generateStmtWithState: IRAssignOp ==="
        exprCode <- generateExprWithState expr
        let opStr = case op of
              Add -> "+="
              Sub -> "-="
              Mul -> "*="
              Div -> "/="
              _ -> error $ "Unsupported compound assignment operator: " ++ show op
        return $ T.concat ["    ", T.pack name, " ", T.pack opStr, " ", exprCode, ";"]

    IRLabel label | "while_" `T.isPrefixOf` T.pack label -> do
        traceM $ "\n=== generateStmtWithState: IRLabel | \"while_\" ==="
        markLabelDeclared label
        trackLabel label  -- Mark while labels as used when declared
        traceM $ "Tracking label usage in IRLabel | \"while_\": " ++ label
        return $ T.concat [T.pack label, ":"]

    IRLabel label -> do
        traceM $ "\n=== generateStmtWithState: IRLabel ==="
        markLabelDeclared label
        st <- get
        if isLabelUsed label st
            then return $ T.concat [T.pack label, ":"]
            else return "" -- Skip unused labels

    IRGoto label | "while_" `T.isPrefixOf` T.pack label -> do
        traceM $ "\n=== generateStmtWithState: IRGoto | \"while_\" ==="
        -- Preserve while loop gotos
        trackLabel label
        traceM $ "Tracking label usage in IRGoto | \"while_\": " ++ label
        return $ T.concat ["    goto ", T.pack label, ";"]

    IRGoto "if_end" -> do
        traceM $ "\n=== generateStmtWithState: IRGoto \"if_end\" ==="
        -- Skip redundant goto if_end when we have a break
        return ""

    IRGoto label -> do
        traceM $ "\n=== generateStmtWithState: IRGoto ==="
        trackLabel label
        traceM $ "Tracking label usage in IRGoto: " ++ label
        return $ T.concat ["    goto ", T.pack label, ";"]

    IRJumpIfTrue cond label -> do
        traceM $ "\n=== generateStmtWithState: IRJumpIfTrue ==="
        trackLabel label  -- Add this line
        traceM $ "Tracking label usage in IRJumpIfTrue: " ++ label
        condCode <- generateExprWithState cond
        return $ T.concat ["    if (", condCode, ") goto ", T.pack label, ";"]

    IRJumpIfZero cond label -> do
        traceM $ "\n=== generateStmtWithState: IRJumpIfZero ==="
        trackLabel label
        st <- get
        traceM $ "\n=== Processing Jump ===\n" ++
                 "Jump target: " ++ label ++ "\n" ++
                 "Used labels after update: " ++ show (cgUsedLabels st)
        condCode <- generateExprWithState cond
        return $ T.concat ["    if (!(", condCode, ")) goto ", T.pack label, ";"]

    IRProcCall "print" [expr] -> do
        traceM $ "\n=== generateStmtWithState: IRProcCall \"print\" ==="
        (value, fmt) <- generatePrintExprWithState expr
        return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]

    IRProcCall name _ -> do
        traceM $ "\n=== generateStmtWithState: IRProcCall ==="
        lift $ Left $ UnsupportedOperation $ T.pack $ "Unsupported procedure: " ++ name

-- Label tracking helpers
trackLabel :: String -> StateT CGState (Either CGenError) ()
trackLabel label = do
    before <- get
    traceM $ "trackLabel - before: " ++ show (cgUsedLabels before)
    modify $ \s -> s { cgUsedLabels = S.insert label (cgUsedLabels s) }
    after <- get
    traceM $ "trackLabel - after: " ++ show (cgUsedLabels after)

markLabelDeclared :: String -> StateT CGState (Either CGenError) ()
markLabelDeclared label = do
    before <- get
    traceM $ "markLabelDeclared - before: " ++ show (cgDeclaredLabels before)
    modify $ \s -> s { cgDeclaredLabels = S.insert label (cgDeclaredLabels s) }
    after <- get
    traceM $ "markLabelDeclared - after: " ++ show (cgDeclaredLabels after)

isLabelUsed :: String -> CGState -> Bool
isLabelUsed label st = S.member label (cgUsedLabels st)

isLabelDeclared :: String -> CGState -> Bool
isLabelDeclared label st = S.member label (cgDeclaredLabels st)

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
generateLiteral (IRBoolLit b) = T.pack $ if b then "1" else "0"
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
            traceM $ "Generating struct literal for: " ++ name
            fieldVals <- mapM generateExprWithState fields

            case fields of
                [IRCall "struct_lit" [IRLit (IRStringLit innerName), innerVal], val2] -> do
                    -- Nested struct initialization - use correct type names
                    let result = T.concat ["(struct ", T.pack name, ") {",
                                         "(struct ", T.pack innerName, ") {",
                                         T.intercalate ", " [genField innerVal], "}, ",
                                         genField val2, "}"]
                    traceM $ "Generated nested struct init: " ++ T.unpack result
                    return result
                _ -> do
                    let result = T.concat ["(struct ", T.pack name, ") {",
                                         T.intercalate ", " fieldVals, "}"]
                    traceM $ "Generated simple struct init: " ++ T.unpack result
                    return result
            where
                genField val = case val of
                    IRLit lit -> generateLiteral lit
                    _ -> T.pack $ show val

        IRCall "print" [expr] -> do
            traceM $ "\n=== generateExpr: print ==="
            traceM $ "Print argument: " ++ show expr
            (value, fmt) <- generatePrintExprWithState expr
            traceM $ "Generated print: value=" ++ T.unpack value ++ ", fmt=" ++ T.unpack fmt
            return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]

        IRCall fname args
            | fname `elem` ["Add", "Sub", "Mul", "Div", "Lt", "Gt", "Eq"] -> do
                -- Keep existing operator handling
                left <- generateExprWithState (head args)
                right <- generateExprWithState (args !! 1)
                let cOp = case fname of
                      "Add" -> "+"
                      "Sub" -> "-"
                      "Mul" -> "*"
                      "Div" -> "/"
                      "Lt" -> "<"
                      "Gt" -> ">"
                      "Eq" -> "=="
                return $ T.concat ["(", left, ") ", T.pack cOp, " (", right, ")"]

            | otherwise -> do
                -- New case: Handle regular function calls
                argExprs <- mapM generateExprWithState args
                return $ T.concat [T.pack fname, "(", T.intercalate ", " argExprs, ")"]

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
