{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zap.Codegen.C
  ( generateC,
    CGenError (..),
  )
where

import Control.Monad (forM_, when)
import Control.Monad.State
import Data.Char (isSpace)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace
import Zap.AST
import Zap.IR

data CGState = CGState
  { cgVarTypes :: M.Map String IRType,
    cgUsedLabels :: S.Set String,
    cgDeclaredLabels :: S.Set String,
    cgPendingReturn :: Bool
  }
  deriving (Show)

data CGenError
  = UnsupportedType IRType
  | UnsupportedOperation T.Text
  deriving (Show, Eq)

generateC :: IRProgram -> Either CGenError T.Text
generateC prog@(IRProgram _funcs) = evalStateT (generateCWithState prog) (CGState M.empty S.empty S.empty False)

generateCWithState :: IRProgram -> StateT CGState (Either CGenError) T.Text
generateCWithState (IRProgram funcs) = do
  lift $ traceM $ "\n=== Starting C Code Generation for IR: " ++ show funcs ++ " ==="

  forM_ funcs $ \(fn, meta) -> do
    lift $ traceM $ "Function " ++ fnName fn ++ " metadata: " ++ show meta

  -- Get symbol table from main function's metadata, or use empty table if not present
  let mainMeta = snd $ last funcs
  lift $ traceM $ "Main function metadata for struct generation: " ++ show mainMeta

  let symTable = case metaSymTable mainMeta of
        Just s -> s
        Nothing -> emptySymbolTable -- Default to empty table instead of error
  lift $ traceM $ "Symbol table: " ++ show symTable

  -- Get struct definitions from last function's metadata (where we store symbol table)
  let structDefs' = generateStructDefinitions symTable [(func, meta) | (func, meta) <- funcs]
  lift $ traceM $ "Generated struct definitions: " ++ show structDefs'

  -- Add isMainFunc at module level since it's used in multiple places
  let declarations =
        map (generateFunctionDeclaration symTable) $
          filter (not . isMainFunc . fst) funcs

  functionDefs <- mapM (generateFunctionWithState symTable) funcs

  let program =
        T.unlines
          [ "#include <stdio.h>",
            "#include <stdint.h>",
            "",
            structDefs', -- Add struct definitions first
            "",
            T.unlines declarations,
            "",
            T.unlines functionDefs
          ]

  lift $ traceM $ "\n=== Final Generated Program ===\n" ++ T.unpack program
  checkUnusedLabels
  return program
  where
    isMainFunc :: IRFuncDecl -> Bool
    isMainFunc (IRFuncDecl {fnName = "main"}) = True
    isMainFunc _ = False

-- generateC :: IRProgram -> Either CGenError T.Text
-- generateC prog = evalStateT (generateCWithState prog) (CGState M.empty S.empty S.empty False)

-- generateCWithState :: IRProgram -> StateT CGState (Either CGenError) T.Text
-- generateCWithState (IRProgram funcs) = do
--     lift $ traceM $ "\n=== Starting C Code Generation for IR: " ++ show funcs ++ " ==="

--     -- Generate struct definitions (keep existing implementation)
--     let structDefs = case funcs of
--           (_, meta):_ -> generateStructDefinitions [(_,meta)]
--           [] -> ""

--     -- Add new: Generate function declarations
--     let declarations = map generateFunctionDeclaration $ filter (not . isMainFunc . fst) funcs

--     lift $ traceM $ "\nFunctions to generate: " ++ show (map (fnName . fst) funcs)
--     -- Generate each function with state
--     functionDefs <- mapM generateFunctionWithState funcs

--     let program = T.unlines
--           [ "#include <stdio.h>"
--           , "#include <stdint.h>"
--           , ""
--           , structDefs
--           , ""
--           , T.unlines declarations  -- Add function declarations
--           , ""
--           , T.unlines functionDefs
--           ]

--     lift $ traceM $ "\n=== Final Generated Program ===\n" ++ T.unpack program

--     checkUnusedLabels

--     return program
--   where
--     isMainFunc (IRFuncDecl {fnName = "main"}) = True
--     isMainFunc _ = False

-- Debug helper for checking unused labels
checkUnusedLabels :: StateT CGState (Either CGenError) ()
checkUnusedLabels = do
  st <- get
  let declared = cgDeclaredLabels st
  let used = cgUsedLabels st
  let unused = S.difference declared used
  when (not $ S.null unused) $
    lift $
      traceM $
        "Warning: Unused labels: " ++ show (S.toList unused)

-- Helper for generating function declarations
generateFunctionDeclaration :: SymbolTable -> (IRFuncDecl, IRMetadata) -> T.Text
generateFunctionDeclaration st (func, _) = do
  let _ = trace "\n=== generateFunctionDeclaration ===" ()
      _ = trace ("Function: " ++ show func) ()
      typeStr = irTypeToC (fnRetType func) st
      _ = trace ("Return type: " ++ T.unpack typeStr) ()
      params =
        T.intercalate ", " $
          map
            ( \(name, irType) -> do
                let paramType = irTypeToC irType st
                let _ = trace ("Parameter " ++ name ++ " type: " ++ T.unpack paramType) ()
                T.concat [paramType, " ", T.pack name]
            )
            (fnParams func)
      _ = trace ("Final declaration: " ++ T.unpack (T.concat [typeStr, " ", T.pack (fnName func), "(", params, ");"]))
  T.concat [typeStr, " ", T.pack (fnName func), "(", params, ");"]

-- Helper to generate struct definitions
generateStructDefinitions :: SymbolTable -> [(IRFuncDecl, IRMetadata)] -> T.Text
generateStructDefinitions st funcs = do
  let _ = trace "\n=== Generating struct definitions ===" ()
      _ = trace ("Input functions: " ++ show funcs) ()
      _ = trace ("Metadata symbol tables: " ++ show [metaSymTable meta | (_, meta) <- funcs]) ()
      structTypes = collectStructTypes funcs
      _ = trace ("Found struct types: " ++ show structTypes) ()
      definitions = map genStructDef structTypes
      _ = trace ("Generated definitions: " ++ show definitions) ()
   in T.unlines $ map rstrip $ map T.unpack definitions
  where
    genStructDef :: (String, [(String, IRType)]) -> T.Text
    genStructDef (name, fields) = do
      let _ = trace ("\n=== genStructDef ===\nName: " ++ name ++ "\nFields: " ++ show fields) ()
      T.unlines $
        map rstrip $
          map T.unpack $
            [ T.concat ["struct ", T.pack name, " {"],
              T.intercalate ";\n" $ map genField fields ++ [""],
              "};",
              ""
            ]

    genField :: (String, IRType) -> T.Text
    genField (fname, ftype) = do
      let typeStr = irTypeToC ftype st
          _ = trace ("\n=== genField ===\nField: " ++ fname ++ "\nType: " ++ show ftype ++ "\nGenerated type: " ++ T.unpack typeStr) ()
      T.concat ["    ", typeStr, " ", T.pack fname]

collectStructTypes :: [(IRFuncDecl, IRMetadata)] -> [(String, [(String, IRType)])]
collectStructTypes funcs = do
  traceM "\n=== collectStructTypes ==="

  -- Get main metadata
  let mainMeta = snd $ last funcs
  traceM $ "Main meta: " ++ show mainMeta

  -- Extract structs from symbol table
  let extractedStructs = case metaSymTable mainMeta of
        Just st -> do
          traceM $ "Found symbol table in main metadata: " ++ show st
          let structs =
                [ (name, fields)
                  | (_, def) <- M.toList (structDefs st),
                    null (structParams def), -- Only collect concrete structs
                    let name = structName def,
                    let fields =
                          [ (fname, IRTypeStruct refName sid) -- KEY CHANGE: Preserve the reference name
                            | (fname, TypeStruct sid refName) <- structFields def
                          ]
                            ++ [ (fname, convertType ftype)
                                 | (fname, ftype) <- structFields def,
                                   not $ isStructType ftype
                               ]
                ]
          traceM $ "Extracted structs: " ++ show structs

          -- Build dependency graph
          let deps =
                [ (name, [n | (_, IRTypeStruct n _) <- fields])
                  | (name, fields) <- structs
                ]
          traceM $ "Dependencies: " ++ show deps

          -- Get ordered names through topological sort
          let ordered = topologicalSort deps
          traceM $ "Ordered names: " ++ show ordered

          -- Create sorted struct list
          let sortedStructs =
                [ s | name <- ordered, s@(n, _) <- structs, n == name
                ]
          traceM $ "Sorted structs: " ++ show sortedStructs

          -- Ensure uniqueness while preserving order
          let uniqueStructs = L.nubBy (\(n1, _) (n2, _) -> n1 == n2) sortedStructs
          traceM $ "Final unique structs: " ++ show uniqueStructs
          uniqueStructs
        Nothing -> []

  traceM $ "Found structs from metadata: " ++ show extractedStructs
  extractedStructs
  where
    isStructType (TypeStruct _ _) = True
    isStructType _ = False

    -- Topological sort implementation
    topologicalSort :: [(String, [String])] -> [String]
    topologicalSort deps =
      let vertices = map fst deps
          edges = [(from, to) | (from, tos) <- deps, to <- tos]
       in reverse $ dfs S.empty vertices edges
      where
        dfs _ [] _ = []
        dfs visited (v : vs) edges
          | v `S.member` visited = dfs visited vs edges
          | otherwise =
              let visited' = S.insert v visited
                  deps' = [u | (_, u) <- edges, u == v]
                  rest = dfs visited' (deps' ++ vs) edges
               in v : rest

    -- Type conversion helper
    convertType :: Type -> IRType
    convertType (TypeNum Int32) = IRTypeInt32
    convertType (TypeNum Int64) = IRTypeInt64
    convertType (TypeNum Float32) = IRTypeFloat32
    convertType (TypeNum Float64) = IRTypeFloat64
    convertType (TypeParam _) = IRTypeInt32 -- Should never happen for specialized structs
    convertType (TypeStruct sid name) = IRTypeStruct name sid
    convertType t = error $ "Unexpected type in struct field: " ++ show t

-- Helper to convert IR types to C types
irTypeToC :: IRType -> SymbolTable -> T.Text
irTypeToC irType st = do
  let _ = trace ("\n=== irTypeToC ===\nType: " ++ show irType ++ "\nSymbol table: " ++ show st) ()
      result = case irType of
        IRTypeInt32 -> "int32_t"
        IRTypeInt64 -> "int64_t"
        IRTypeFloat32 -> "float"
        IRTypeFloat64 -> "double"
        IRTypeStruct name sid -> do
          let structName' = case (M.lookup name (structNames st), lookupStruct sid st) of
                -- Generic struct - find all specializations
                (Just _, Just def)
                  | not (null (structParams def)) ->
                      -- Look for any specialized version by finding matching prefix
                      let prefix = T.pack (name ++ "_")
                          specialized =
                            filter
                              (\(n, _) -> prefix `T.isPrefixOf` n)
                              (M.toList $ M.mapKeys T.pack $ structNames st)
                       in case specialized of
                            (specializedName, _) : _ -> T.unpack specializedName
                            [] -> name
                (Just _, _) -> name -- Concrete struct
                (Nothing, _) -> getSpecializedStructName name sid st
          let _ = trace ("Found struct name: " ++ structName') ()
          T.concat ["struct ", T.pack structName']
        IRTypeVoid -> "void"
        IRTypeVar _ -> do
          -- Look at the funcDefs to find a specialized version
          let funcNames = M.keys (funcDefs st)
          -- Match any specialized version by looking for _suffix pattern
          let suffixes = ["i32", "i64", "f32", "f64"] -- From typeToSuffix
          let specialized =
                [ name | name <- funcNames, suffix <- suffixes, T.isSuffixOf (T.pack ("_" ++ suffix)) (T.pack name)
                ]
          let _ = trace ("Found specialized functions: " ++ show specialized) ()

          case specialized of
            (specName : _) -> case M.lookup specName (funcDefs st) of
              Just specDef -> case funcParams specDef of
                (Param _ paramType) : _ -> convertType paramType
                _ -> "void"
              Nothing -> "void"
            [] -> "void"
        _ -> error $ "Unrecognized IRType: " ++ show irType ++ " unable to convert to C type."
  trace ("Generated C type: " ++ T.unpack result) result
  where
    -- Helper to convert AST Type to C type string
    convertType :: Type -> T.Text
    convertType (TypeNum Int32) = "int32_t"
    convertType (TypeNum Int64) = "int64_t"
    convertType (TypeNum Float32) = "float"
    convertType (TypeNum Float64) = "double"
    convertType _ = "void"

generateFunctionWithState :: SymbolTable -> (IRFuncDecl, IRMetadata) -> StateT CGState (Either CGenError) T.Text
generateFunctionWithState st (func, _) = do
  traceM $ "\n=== generateFunctionWithState: " ++ show func ++ " ==="

  case fnRetType func of
    IRTypeVar _ -> do
      traceM $ "Skipping generic function"
      return ""
    IRTypeStruct name _ -> do
      -- For struct constructor functions, ensure we return an initialized struct
      let params = fnParams func
      let paramNames = map fst params
      -- Extract the actual parameter type instead of defaulting to int
      let paramTypes = map snd params
      let paramDecls =
            zipWith
              ( \name' typ ->
                  T.concat
                    [ case typ of
                        -- Use specialized type name for struct parameters
                        IRTypeStruct structName' _ ->
                          let specializedName =
                                if "Box" == structName'
                                  then "Box_i32" -- Use concrete type
                                  else structName'
                           in T.concat ["struct ", T.pack specializedName, " ", T.pack name']
                        _ -> T.concat [irTypeToC typ st, " ", T.pack name']
                    ]
              )
              paramNames
              paramTypes
      let structInit =
            T.concat
              [ "    struct ",
                T.pack name,
                " result = {",
                T.intercalate ", " (map T.pack paramNames),
                "};\n",
                "    return result;"
              ]
      let signature =
            T.concat
              [ "struct ",
                T.pack name,
                " ",
                T.pack (fnName func),
                "(",
                T.intercalate ", " paramDecls,
                ")"
              ]
      return $ T.unlines [signature <> " {", structInit, "}"]
    _ -> do
      body <- generateBlockWithState st (fnBody func)
      traceM $ "Converted body: " ++ show body

      -- NEW: Check if this is a specialized function
      let isSpecialized = '_' `elem` fnName func
      let specializedType =
            if isSpecialized
              then case drop 1 $ dropWhile (/= '_') $ fnName func of
                "i32" -> Just IRTypeInt32
                "i64" -> Just IRTypeInt64
                "f32" -> Just IRTypeFloat32
                "f64" -> Just IRTypeFloat64
                _ -> Nothing
              else Nothing
      traceM $ "Specialized type: " ++ show specializedType

      -- Determine return type (extended)
      let typeStr = case fnRetType func of
            IRTypeInt32 -> "int32_t"
            IRTypeVoid -> "void"
            _ -> "int" -- Default to int for now
      traceM $ "Type str: " ++ show typeStr

      -- Add implicit return for main if needed
      let needsImplicitReturn = fnName func == "main" && not (any isReturnStmt (irBlockStmts $ fnBody func))

      let finalBody = if needsImplicitReturn then body <> "    return 0;\n" else body

      traceM $ "Final function body: " ++ show finalBody

      -- Generate function signature (unchanged)
      let signature = case fnName func of
            "main" -> T.pack "int main(void)"
            _ -> T.concat [typeStr, " ", T.pack (fnName func), "(", generateParams specializedType (fnParams func), ")"]

      traceM $ "Signature: " ++ show signature

      -- Return function text (unchanged)
      let result = T.unlines $ map rstrip $ map T.unpack $ [signature <> T.pack " {", finalBody, "}"]

      traceM $ "Final generated function: " ++ show result

      return $ result
  where
    -- Helper to check if statement is a return
    isReturnStmt :: (IRStmt, IRMetadata) -> Bool
    isReturnStmt (IRReturn _, _) = True
    isReturnStmt _ = False

    -- Helper to generate parameter list (modified to handle specialization)
    generateParams :: Maybe IRType -> [(String, IRType)] -> T.Text
    generateParams _ [] = T.pack "void"
    generateParams mSpecType params = T.intercalate ", " $ map (formatParam mSpecType) params

    -- Helper to format individual parameters (modified)
    formatParam :: Maybe IRType -> (String, IRType) -> T.Text
    formatParam mSpecType (name, typ) = case typ of
      IRTypeVar _ -> case mSpecType of
        Just concrete -> T.concat [irTypeToC concrete st, " ", T.pack name]
        Nothing -> T.concat ["void ", T.pack name]
      _ -> T.concat [paramTypeToC typ, " ", T.pack name]

    -- Helper to convert IR types to C types (unchanged)
    paramTypeToC :: IRType -> T.Text
    paramTypeToC IRTypeInt32 = "int32_t"
    paramTypeToC IRTypeVoid = "void"
    paramTypeToC _ = "int" -- Default to int for now

generateBlockWithState :: SymbolTable -> IRBlock -> StateT CGState (Either CGenError) T.Text
generateBlockWithState st (IRBlock _ stmts) = do
  lift $ traceM "\n=== generateBlockWithState ==="
  lift $ traceM $ "Input statements: " ++ show stmts

  case stmts of
    -- Match the full while loop pattern including contents
    ((IRLabel label, _) : rest) | "while_" `T.isPrefixOf` T.pack label -> do
      let (loopBody, endStmts) =
            span
              ( \(stmt, _) -> case stmt of
                  IRLabel _ -> not ("_end" `T.isSuffixOf` T.pack label)
                  _ -> True
              )
              rest

      -- Generate while loop condition
      case loopBody of
        ((IRJumpIfZero cond target, _) : bodyStmts) | "_end" `T.isSuffixOf` T.pack target -> do
          -- First generate loop condition
          condCode <- generateExprWithState st cond

          -- Then generate body statements
          bodyCode <- generateStmtsUntilReturn bodyStmts st
          remainingCode <- generateStmtsUntilReturn endStmts st

          return $
            T.unlines $
              filter (not . T.null) $
                [ T.pack "    do {",
                  T.concat ["        if (", condCode, ") break;"]
                ]
                  ++ map ("        " `T.append`) bodyCode
                  ++ [ T.pack "    } while(1);"
                     ]
                  ++ remainingCode
        _ -> do
          -- Fall through to normal statement handling
          stmts' <- generateStmtsUntilReturn stmts st
          return $ T.unlines $ filter (not . T.null) stmts'
    _ -> case isIfElsePattern stmts of
      Just (cond, thenStmts, elseStmts, remaining) -> do
        condCode <- generateExprWithState st cond
        thenCode <- generateStmtsUntilReturn thenStmts st
        elseCode <- generateStmtsUntilReturn elseStmts st

        let ifBlock =
              T.unlines
                [ T.concat ["    if (", condCode, ") {"],
                  T.unlines $ map ("        " `T.append`) thenCode,
                  "    } else {",
                  T.unlines $ map ("        " `T.append`) elseCode,
                  "    }"
                ]

        restCode <- mapM (generateStmtWithState st) remaining
        return $ T.unlines $ filter (not . T.null) $ ifBlock : map T.strip restCode
      Nothing -> do
        stmts' <- generateStmtsUntilReturn stmts st
        return $ T.unlines $ filter (not . T.null) stmts'

-- Helper to generate statements but stop at first return
generateStmtsUntilReturn :: [(IRStmt, IRMetadata)] -> SymbolTable -> StateT CGState (Either CGenError) [T.Text]
generateStmtsUntilReturn [] _ = return []
generateStmtsUntilReturn ((stmt, meta) : rest) symTable = do
  traceM $ "\n=== generateStmtsUntilReturn: " ++ show ((stmt, meta) : rest) ++ " ==="
  code <- generateStmtWithState symTable (stmt, meta)
  traceM $ "Generated statement: " ++ show code
  case stmt of
    IRReturn _ -> do
      traceM $ "Stopping at return statement"
      return [code] -- Stop at return
    IRStmtExpr _ -> do
      traceM $ "Found expression statement"
      st <- get
      if cgPendingReturn st -- Fixed pattern guard issue
        then do
          traceM $ "Pending return set to true in state. Stopping."
          return [code] -- Stop after break->return
        else do
          traceM $ "No pending return found in state. Continuing."
          restCode <- generateStmtsUntilReturn rest symTable
          return $ if T.null code then restCode else code : restCode
    stmt' -> do
      traceM $ "Found statement: " ++ show stmt'
      restCode <- generateStmtsUntilReturn rest symTable
      return $ if T.null code then restCode else code : restCode

generateStmtWithState :: SymbolTable -> (IRStmt, IRMetadata) -> StateT CGState (Either CGenError) T.Text
generateStmtWithState st (stmt, _) = do
  traceM $ "\n=== generateStmtWithState: " ++ show stmt ++ " ==="
  case stmt of
    -- Struct handling (unchanged)
    IRVarDecl name irType@(IRTypeStruct structName' _) initExpr -> do
      traceM $ "\n=== generateStmtWithState: IRVarDecl | IRTypeStruct ==="
      modify $ \s ->
        s
          { cgVarTypes =
              M.insert "current_struct_type" irType $
                M.insert name irType $
                  cgVarTypes s
          }
      exprCode <- generateExprWithState st initExpr
      modify $ \s -> s {cgVarTypes = M.delete "current_struct_type" (cgVarTypes s)}
      return $ T.concat ["    struct ", T.pack structName', " ", T.pack name, " = ", exprCode, ";"]

    -- Regular variable declarations (unchanged)
    IRVarDecl name irType initExpr -> do
      traceM $ "\n=== generateStmtWithState: IRVarDecl ==="
      case initExpr of
        IRCall "struct_lit" (IRLit (IRStringLit structName) : _) -> do
          modify $ \s -> s {cgVarTypes = M.insert name (IRTypeStruct structName (StructId 0)) (cgVarTypes s)}
          exprCode <- generateExprWithState st initExpr
          return $ T.concat ["    struct ", T.pack structName, " ", T.pack name, " = ", exprCode, ";"]
        _ -> do
          modify $ \s -> s {cgVarTypes = M.insert name irType (cgVarTypes s)}
          exprCode <- generateExprWithState st initExpr
          let typeStr = irTypeToC irType st
          return $ T.concat ["    ", typeStr, " ", T.pack name, " = ", exprCode, ";"]

    -- Return statements
    IRReturn Nothing -> do
      traceM $ "\n=== generateStmtWithState: IRReturn Nothing ==="
      return "    return 0;"
    IRReturn (Just expr) -> do
      traceM $ "\n=== generateStmtWithState: IRReturn ==="
      exprCode <- generateExprWithState st expr
      return $ T.concat ["    return ", exprCode, ";"]

    -- Assignment statements (unchanged)
    IRAssign name expr -> do
      traceM $ "\n=== generateStmtWithState: IRAssign ==="
      exprCode <- generateExprWithState st expr
      return $ T.concat ["    ", T.pack name, " = ", exprCode, ";"]
    IRAssignOp name op expr -> do
      traceM $ "\n=== generateStmtWithState: IRAssignOp ==="
      exprCode <- generateExprWithState st expr
      let opStr = case op of
            Add -> "+="
            Sub -> "-="
            Mul -> "*="
            Div -> "/="
            _ -> error $ "Unsupported compound assignment operator: " ++ show op
      return $ T.concat ["    ", T.pack name, " ", T.pack opStr, " ", exprCode, ";"]

    -- While loop labels
    IRLabel label | "while_" `T.isPrefixOf` T.pack label -> do
      traceM $ "\n=== generateStmtWithState: IRLabel | \"while_\" ==="
      markLabelDeclared label
      return $ T.concat [T.pack label, ":"]
    IRLabel label -> do
      traceM $ "\n=== generateStmtWithState: IRLabel ==="
      markLabelDeclared label
      st' <- get
      if isLabelUsed label st'
        then return $ T.concat [T.pack label, ":"]
        else return "" -- Skip unused labels

    -- While loop gotos
    IRGoto label | "while_" `T.isPrefixOf` T.pack label -> do
      traceM $ "\n=== generateStmtWithState: IRGoto | \"while_\" ==="
      traceM $ "Label: " ++ label
      st' <- get
      traceM $ "Current state: " ++ show st'
      trackLabel label
      return $ T.concat ["    goto ", T.pack label, ";"]

    -- Function-level breaks become returns
    IRGoto label
      | not ("while_" `T.isPrefixOf` T.pack label)
          && not ("if_" `T.isPrefixOf` T.pack label) -> do
          traceM $ "\n=== generateStmtWithState: IRGoto (function return) ==="
          -- Mark that we want the next expression to be a return
          modify $ \s -> s {cgPendingReturn = True}
          return ""
    IRGoto label -> do
      traceM $ "\n=== generateStmtWithState: IRGoto ==="
      trackLabel label
      traceM $ "Tracking label usage in IRGoto: " ++ label
      return $ T.concat ["    goto ", T.pack label, ";"]

    -- Expression statements
    IRStmtExpr expr -> do
      traceM $ "\n=== generateStmtWithState: IRStmtExpr ==="
      traceM $ "Converting StmtExpr: " ++ show expr
      exprCode <- generateExprWithState st expr
      -- Check if this should be a return
      st' <- get
      if cgPendingReturn st'
        then do
          -- Reset pending return state
          modify $ \s -> s {cgPendingReturn = False}
          return $ T.concat ["    return ", exprCode, ";"]
        else -- Always return expression values in function context
          return $ T.concat ["    return ", exprCode, ";"]

    -- Conditional jumps for while loops
    IRJumpIfZero cond label | "while_" `T.isPrefixOf` T.pack label -> do
      traceM $ "\n=== generateStmtWithState: IRJumpIfZero ==="
      trackLabel label
      condCode <- generateExprWithState st cond
      return $ T.concat ["    if (!(", condCode, ")) goto ", T.pack label, ";"]
    IRJumpIfZero cond label -> do
      traceM $ "\n=== generateStmtWithState: IRJumpIfZero ==="
      trackLabel label
      st' <- get
      traceM $
        "\n=== Processing Jump ===\n"
          ++ "Jump target: "
          ++ label
          ++ "\n"
          ++ "Used labels after update: "
          ++ show (cgUsedLabels st')
      condCode <- generateExprWithState st cond
      return $ T.concat ["    if (!(", condCode, ")) goto ", T.pack label, ";"]
    -- Print statements (unchanged)
    IRProcCall "print" [expr] -> do
      traceM $ "\n=== generateStmtWithState: IRProcCall \"print\" ==="
      (value, fmt) <- generatePrintExprWithState st expr
      return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]
    IRProcCall name _ -> do
      traceM $ "\n=== generateStmtWithState: IRProcCall ==="
      traceM $ "Converting proc call: " ++ name
      lift $ Left $ UnsupportedOperation $ T.pack $ "Unsupported procedure: " ++ name

    -- Skip all other labels/gotos/jumps as they're handled by if/else generation
    _ -> return ""

-- Label tracking helpers
trackLabel :: String -> StateT CGState (Either CGenError) ()
trackLabel label = do
  before <- get
  traceM $ "trackLabel - before: " ++ show (cgUsedLabels before)
  modify $ \s -> s {cgUsedLabels = S.insert label (cgUsedLabels s)}
  after <- get
  traceM $ "trackLabel - after: " ++ show (cgUsedLabels after)

markLabelDeclared :: String -> StateT CGState (Either CGenError) ()
markLabelDeclared label = do
  before <- get
  traceM $ "markLabelDeclared - before: " ++ show (cgDeclaredLabels before)
  modify $ \s -> s {cgDeclaredLabels = S.insert label (cgDeclaredLabels s)}
  after <- get
  traceM $ "markLabelDeclared - after: " ++ show (cgDeclaredLabels after)

isLabelUsed :: String -> CGState -> Bool
isLabelUsed label st = S.member label (cgUsedLabels st)

-- Helper for generating literal values
generateLiteral :: IRLiteral -> T.Text
generateLiteral (IRInt32Lit n) = T.pack $ show n
generateLiteral (IRInt64Lit n) = T.pack $ show n ++ "L"
generateLiteral (IRFloat32Lit n) = T.pack (show n ++ "f")
generateLiteral (IRFloat64Lit n) = T.pack $ show n
generateLiteral (IRBoolLit b) = T.pack $ if b then "1" else "0"
generateLiteral (IRVarRef name) = T.pack name
generateLiteral (IRStringLit s) = T.concat ["\"", T.pack s, "\""]

generateExprWithState :: SymbolTable -> IRExpr -> StateT CGState (Either CGenError) T.Text
generateExprWithState st expr = do
  traceM $ "\n=== generateExpr called with: " ++ show expr
  case expr of
    IRCall "field_access" [baseExpr, IRLit (IRStringLit field)] -> do
      traceM $ "\n=== generateExpr: field_access ==="
      base <- generateExprWithState st baseExpr
      let result = T.concat [base, ".", T.pack field]
      traceM $ "Generated field access: " ++ T.unpack result
      return result
    IRCall "struct_lit" (IRLit (IRStringLit name) : fields) -> do
      traceM $ "Generating struct literal for: " ++ name
      fieldVals <- mapM (generateExprWithState st) fields

      case fields of
        [IRCall "struct_lit" [IRLit (IRStringLit innerName), innerVal], val2] -> do
          -- Nested struct initialization - use correct type names
          let result =
                T.concat
                  [ "(struct ",
                    T.pack name,
                    ") {",
                    "(struct ",
                    T.pack innerName,
                    ") {",
                    T.intercalate ", " [genField innerVal],
                    "}, ",
                    genField val2,
                    "}"
                  ]
          traceM $ "Generated nested struct init: " ++ T.unpack result
          return result
        _ -> do
          let result =
                T.concat
                  [ "(struct ",
                    T.pack name,
                    ") {",
                    T.intercalate ", " fieldVals,
                    "}"
                  ]
          traceM $ "Generated simple struct init: " ++ T.unpack result
          return result
      where
        genField val = case val of
          IRLit lit -> generateLiteral lit
          _ -> T.pack $ show val
    IRCall "print" [expr'] -> do
      traceM $ "\n=== generateExpr: print ==="
      traceM $ "Print argument: " ++ show expr'
      (value, fmt) <- generatePrintExprWithState st expr'
      traceM $ "Generated print: value=" ++ T.unpack value ++ ", fmt=" ++ T.unpack fmt
      return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]
    IRCall "Not" conds -> do
      condExprs <- mapM (generateExprWithState st) conds
      return $ T.intercalate " && " condExprs
    IRCall fname args
      | fname `elem` ["Add", "Sub", "Mul", "Div", "Lt", "Gt", "Eq"] -> do
          -- Keep existing operator handling
          case args of
            left : right : _ -> do
              leftCode <- generateExprWithState st left
              rightCode <- generateExprWithState st right
              case operatorToC fname of
                Right cOp ->
                  return $ T.concat ["(", leftCode, ") ", cOp, " (", rightCode, ")"]
                Left err -> lift $ Left err
            _ ->
              lift $
                Left $
                  UnsupportedOperation $
                    T.pack $
                      "Binary operator " ++ fname ++ " requires two arguments"
      | otherwise -> do
          -- New case: Handle regular function calls
          argExprs <- mapM (generateExprWithState st) args
          return $ T.concat [T.pack fname, "(", T.intercalate ", " argExprs, ")"]
    IRLit lit -> return $ generateLiteral lit
    IRVar name -> return $ T.pack name

operatorToC :: String -> Either CGenError T.Text
operatorToC = \case
  "Add" -> Right "+"
  "Sub" -> Right "-"
  "Mul" -> Right "*"
  "Div" -> Right "/"
  "Lt" -> Right "<"
  "Gt" -> Right ">"
  "Eq" -> Right "=="
  op -> Left $ UnsupportedOperation $ T.pack $ "Unsupported operator: " ++ op

-- Helper to determine format specifier
generatePrintExprWithState :: SymbolTable -> IRExpr -> StateT CGState (Either CGenError) (T.Text, T.Text)
generatePrintExprWithState st expr = do
  traceM $ "\n=== generatePrintExpr ==="
  traceM $ "Input expression: " ++ show expr
  case expr of
    IRCall "field_access" [baseExpr, IRLit (IRStringLit field)] -> do
      traceM $ "Processing field access:"
      traceM $ "  Base expr: " ++ show baseExpr
      traceM $ "  Field: " ++ field
      base <- generateExprWithState st baseExpr
      traceM $ "  Generated base: " ++ T.unpack base

      -- Get stored type information for the variable
      st' <- get
      let fmt = case baseExpr of
            IRVar name -> case M.lookup name (cgVarTypes st') of
              Just (IRTypeStruct sname _) ->
                if "_i64" `T.isSuffixOf` T.pack sname
                  then "%ld"
                  else
                    if "_f32" `T.isSuffixOf` T.pack sname
                      then "%f"
                      else
                        if "_f64" `T.isSuffixOf` T.pack sname
                          then "%lf"
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
      st' <- get
      let fmt = case M.lookup name (cgVarTypes st') of
            Just IRTypeInt32 -> "%d"
            Just IRTypeInt64 -> "%ld"
            Just IRTypeFloat32 -> "%f"
            Just IRTypeFloat64 -> "%lf"
            _ -> "%d" -- Fallback
      traceM $ "Variable " ++ name ++ " type format: " ++ T.unpack fmt
      return (T.pack name, fmt)
    IRLit (IRVarRef name) ->
      return (T.pack name, "%d")
    IRCall op [e1, e2] -> do
      left <- generateExprWithState st e1
      right <- generateExprWithState st e2
      case op of
        "Lt" -> return (T.concat ["((", left, ") < (", right, ") ? 1 : 0)"], "%d")
        "Gt" -> return (T.concat ["((", left, ") > (", right, ") ? 1 : 0)"], "%d")
        "Eq" -> return (T.concat ["((", left, ") == (", right, ") ? 1 : 0)"], "%d")
        "NotEq" -> return (T.concat ["((", left, ") != (", right, ") ? 1 : 0)"], "%d")
        _ -> do
          argCode <- mapM (generateExprWithState st) [e1, e2]
          let call = T.concat [T.pack op, "(", T.intercalate ", " argCode, ")"]
          return (call, "%d")
    expr' -> do
      exprCode <- generateExprWithState st expr'
      return (exprCode, "%d")

isIfElsePattern ::
  [(IRStmt, IRMetadata)] ->
  Maybe
    ( IRExpr, -- condition
      [(IRStmt, IRMetadata)], -- then statements
      [(IRStmt, IRMetadata)], -- else statements
      [(IRStmt, IRMetadata)] -- remaining statements
    )
isIfElsePattern ((IRJumpIfZero (IRCall "Not" [cond]) label, _) : rest) = do
  traceM "\n=== isIfElsePattern ===\n"
  traceM $ "Found Not-wrapped condition"
  traceM $ "Cond: " ++ show cond
  traceM $ "Label: " ++ show label
  traceM $ "Rest: " ++ show rest

  -- Split at the else label
  let (beforeElse, afterElse) =
        break
          ( \(stmt, _) -> case stmt of
              IRLabel l -> l == label
              _ -> False
          )
          rest

  -- Handle early returns in the then branch
  let thenBody = beforeElse
  let elseBody = case afterElse of
        (_ : rest') ->
          -- Skip the else label
          takeWhile
            ( \(stmt, _) -> case stmt of
                IRLabel _ -> False
                IRGoto _ -> False
                _ -> True
            )
            rest'
        _ -> []

  Just (cond, thenBody, elseBody, [])
isIfElsePattern ((IRJumpIfZero cond label, _) : rest) = do
  traceM "\n=== isIfElsePattern ==="
  traceM $ "Cond: " ++ show cond
  traceM $ "Label: " ++ show label
  traceM $ "Rest: " ++ show rest

  -- Split at the else label
  let (beforeElse, afterElse) =
        break
          ( \(stmt, _) -> case stmt of
              IRLabel l -> l == label
              _ -> False
          )
          rest

  traceM $ "Before else: " ++ show beforeElse
  traceM $ "After else: " ++ show afterElse

  -- Handle early returns in the then branch
  let thenBody = beforeElse -- Include return statement in then branch

  -- Keep expressions after the label but before another label/goto
  let elseBody = case afterElse of
        (_ : rest') ->
          -- Skip the else label
          takeWhile
            ( \(stmt, _) -> case stmt of
                IRLabel _ -> False
                IRGoto _ -> False
                _ -> True
            )
            rest'
        _ -> []

  traceM $ "Then body: " ++ show thenBody
  traceM $ "Else body: " ++ show elseBody

  Just (cond, thenBody, elseBody, [])
isIfElsePattern s = do
  traceM "\n=== isIfElsePattern - unrecognized ==="
  traceM $ "Stmt: " ++ show s
  Nothing

rstrip :: String -> T.Text
rstrip = T.pack . reverse . dropWhile isSpace . reverse
