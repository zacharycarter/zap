{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Zap.Codegen.C
  ( generateC
  , CGenError(..)
  , CodegenState(..)
  , irTypeToCType
  , generateFuncDef
  , generateStructDef
  , generateTypedExpr
  , generateStmt
  , Codegen
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Control.Monad (forM, forM_, when)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Debug.Trace
import Zap.IR.Core

data CGenError = UnsupportedType IRType
  | UnsupportedOperation IROp IRType IRType
  | ParseError String
  deriving (Show, Eq)

type Codegen = ExceptT CGenError (State CodegenState)

data CodegenState = CodegenState
  { varEnv :: M.Map T.Text IRType
  , funcEnv :: M.Map T.Text (IRType, [IRType])
  , blockCounter :: Int
  , structDefs :: [Text]
  }

generateC :: IR -> Either CGenError Text
generateC (IRProgram decls exprs) = evalState (runExceptT $ do
    traceM "\n=== Starting Complete Program Generation ==="

    traceM "\n--- Processing Declarations ---"
    declsText <- forM decls $ \decl -> do
        traceM $ "\nProcessing declaration:\n" ++ show decl
        case decl of
            IRStruct name fields -> do
                traceM $ "Generating struct definition '" ++ T.unpack name ++ "'"
                res <- generateStructDef decl
                traceM $ "Generated struct:\n" ++ T.unpack res
                return res
            IRFunc name params retType body -> do
                traceM $ "Generating function '" ++ T.unpack name ++ "'"
                traceM $ "Parameters: " ++ show params
                traceM $ "Return type: " ++ show retType
                traceM $ "Body: " ++ show body
                res <- generateFuncDef decl
                traceM $ "Generated function:\n" ++ T.unpack res
                return res

    traceM "\n--- Processing Main Function Expressions ---"
    mainBody <- forM exprs $ \expr -> do
        traceM $ "\nGenerating statement for:\n" ++ show expr
        res <- generateStmt expr
        traceM $ "Generated statement:\n" ++ T.unpack res
        return res

    traceM "\n--- Assembling Complete Program ---"
    let vectorOps = T.unlines
          [ "typedef struct { float x, y, z; } v3f32;"
          , "v3f32 vec3_add(v3f32 a, v3f32 b) {"
          , "    return (v3f32){a.x + b.x, a.y + b.y, a.z + b.z};"
          , "}"
          , ""
          ]

    let program = T.unlines
          [ T.pack "#include <stdio.h>"
          , T.pack "#include <stdint.h>"
          , T.pack "#include <immintrin.h>"
          , T.pack ""
          , vectorOps
          , T.unlines declsText
          , T.pack "int main(void) {"
          , T.unlines mainBody
          , T.pack "    return 0;"
          , T.pack "}"
          ]

    traceM $ "\n=== Final Generated Program ===\n" ++ T.unpack program
    return program
    ) initState
  where
    initState = CodegenState M.empty M.empty 0 []

generateTypedExpr :: IRExpr -> Codegen (Text, IRType)
generateTypedExpr irExpr = do
    traceM $ "Generating expression: " ++ show irExpr
    -- Use pattern matching to deconstruct IRExpr
    let IRExpr meta exprNode = irExpr
    case exprNode of
        -- This case should go first
        IRBlock _ exprs mResult -> do
            -- Generate all statements before the last
            when (not (null exprs)) $ do
                mapM_ generateStmt (init exprs)
            -- Get value of last expression 
            case last exprs of
                lastExpr -> do
                    (val, typ) <- generateTypedExpr lastExpr
                    return (val, typ)
        IRNum t val ->
            return (val, IRTypeNum t)

        IRString s ->
            return (T.concat ["\"", s, "\""], IRTypeString)

        IRVar name -> do
            varType <- gets (M.lookup name . varEnv)
            case varType of
                Just t -> return (name, t)
                Nothing -> throwError $ UnsupportedType IRTypeString

        IRVec vt components -> do
            componentVals <- mapM generateTypedExpr components
            let vals = map fst componentVals
            case vt of
                IRVec4 IRFloat32 ->
                    let simdInit = T.concat ["_mm_set_ps(", T.intercalate ", " (reverse vals), ")"]
                    in return (simdInit, IRTypeVec vt)
                IRVec3 IRFloat32 ->
                    let structInit = T.concat ["(v3f32){", T.intercalate ", " vals, "}"]
                    in return (structInit, IRTypeVec vt)
                _ -> throwError $ UnsupportedType (IRTypeVec vt)

        IRBinOp op lhs rhs -> do
            (lval, ltyp) <- generateTypedExpr lhs
            (rval, rtyp) <- generateTypedExpr rhs
            case (op, ltyp, rtyp) of
                -- Add struct-based vector operations
                (IRAdd, IRTypeStruct "Vec3" fields1, IRTypeStruct "Vec3" fields2) ->
                    return (T.concat ["vec3_add(", lval, ", ", rval, ")"], ltyp)

                -- Existing vector operations remain unchanged
                (IRAdd, IRTypeVec (IRVec4 IRFloat32), IRTypeVec (IRVec4 IRFloat32)) ->
                    return (T.concat ["_mm_add_ps(", lval, ", ", rval, ")"], ltyp)

                (IRDot, IRTypeVec (IRVec4 IRFloat32), IRTypeVec (IRVec4 IRFloat32)) ->
                    return (T.concat ["_mm_cvtss_f32(_mm_dp_ps(", lval, ", ", rval, ", 0xFF))"], IRTypeNum IRFloat32)

                (IRAdd, IRTypeVec (IRVec3 IRFloat32), IRTypeVec (IRVec3 IRFloat32)) ->
                    return (T.concat ["vec3_add(", lval, ", ", rval, ")"], IRTypeVec (IRVec3 IRFloat32))

                -- Existing numeric operations remain unchanged
                (IRAdd, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " + ", rval, ")"], IRTypeNum t1)

                (IRSub, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " - ", rval, ")"], IRTypeNum t1)

                (IRMul, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " * ", rval, ")"], IRTypeNum t1)

                (IRDiv, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " / ", rval, ")"], IRTypeNum t1)

                -- Assignment operators
                (IRAddAssign, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " += ", rval, ")"], IRTypeNum t1)

                -- Comparison operators
                (IRLt, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " < ", rval, ")"], IRTypeBool)

                (IRGt, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " > ", rval, ")"], IRTypeBool)
                   
                (IREq, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " == ", rval, ")"], IRTypeBool)

                -- Handle incompatible vector types
                (_, IRTypeVec v1, IRTypeVec v2) | v1 /= v2 ->
                    throwError $ UnsupportedOperation op ltyp rtyp

                -- Handle incompatible struct types
                (_, IRTypeStruct n1 _, IRTypeStruct n2 _) | n1 /= n2 ->
                    throwError $ UnsupportedOperation op ltyp rtyp

                _ -> throwError $ UnsupportedOperation op ltyp rtyp

        IRFieldAccess base field -> do
            (baseVal, baseType) <- generateTypedExpr base
            case baseType of
                IRTypeStruct sName fields ->
                    case lookup field fields of
                        Just fType -> do
                            traceM $ "Accessing field " ++ T.unpack field ++ " of type " ++ show fType
                            return (T.concat [baseVal, ".", field], fType)
                        Nothing -> do
                            traceM $ "Field " ++ T.unpack field ++ " not found in struct " ++ T.unpack sName
                            throwError $ UnsupportedType (IRTypeStruct sName [])

                IRTypeVec (IRVec3 IRFloat32) -> do
                    let compIndex = case field of
                            "x" -> "0"
                            "y" -> "1"
                            "z" -> "2"
                            _ -> error $ "Invalid field " ++ T.unpack field ++ " for vec3"
                    return (T.concat [baseVal, ".", compIndex], IRTypeNum IRFloat32)

                IRTypeVec (IRVec4 IRFloat32) -> do
                    -- Extract a single component from __m128
                    let idx = case field of
                                "x" -> "0"
                                "y" -> "1"
                                "z" -> "2"
                                "w" -> "3"
                                _ -> error $ "Invalid component: " ++ T.unpack field
                    -- Use shuffle/extract
                    let code = T.concat ["_mm_extract_ps(", baseVal, ", ", idx, ")"]
                    return (code, IRTypeNum IRFloat32)

                _ -> throwError $ UnsupportedType IRTypeString

        IRStructLit name fields -> do
            fieldVals <- mapM (\(fn, fe) -> (fn,) <$> generateTypedExpr fe) fields
            let (fNames, fExprsTypes) = unzip fieldVals
            let exprVals = map fst fExprsTypes
            let fieldTypes = map snd fExprsTypes
            let structType = IRTypeStruct name (zip fNames fieldTypes)
            cType <- irTypeToCType structType
            let assignments = T.intercalate ", " (zipWith (\n v -> n <> ": " <> v) fNames exprVals)
            return ("(" <> cType <> "){" <> assignments <> "}", structType)

        IRLetAlloc name val strat -> do
            (valExpr, valType) <- generateTypedExpr val
            typeStr <- irTypeToCType valType
            modify $ \s -> s { varEnv = M.insert name valType (varEnv s) }
            return (T.concat [typeStr, " ", name, " = ", valExpr, ";"], valType)

        IRPrint printExpr -> do
            traceM $ "Generating print statement for expr: " ++ show printExpr
            (val, typ) <- generateTypedExpr printExpr
            traceM $ "Print expression generated with type: " ++ show typ
            fmt <- getPrintfFormat typ
            traceM $ "Selected printf format: " ++ show fmt
            let printStmt = case typ of
                    IRTypeVec (IRVec3 _) -> T.concat ["printf(\"", fmt, "\\n\", ", val, ".x, ", val, ".y, ", val, ".z)"]
                    IRTypeVec (IRVec4 _) -> T.concat ["printf(\"", fmt, "\\n\", ", val, ")"]
                    _ -> T.concat ["printf(\"", fmt, "\\n\", ", val, ")"]
            return (printStmt, IRTypeVoid)

        IRBlockAlloc name exprs mResult -> do
            stmts <- mapM generateStmt exprs
            res <- case mResult of
                Just r -> do
                    (rv, rt) <- generateTypedExpr r
                    return (rv, rt)
                Nothing -> return ("", IRTypeNum IRInt32)
            return (T.unlines stmts <> fst res, snd res)

        IRCall name args -> do
            traceM $ "Generating function call: " ++ T.unpack name
            state <- get
            argVals <- mapM generateTypedExpr args
            case (M.lookup name (varEnv state), M.lookup name (funcEnv state)) of
                -- Struct constructor case
                (Just (IRTypeStruct sName fields), _) -> do
                    let fieldNames = map fst fields
                    let argStrs = map fst argVals
                    let initStrs = T.intercalate ", " $
                          zipWith (\f v -> f <> ": " <> v) fieldNames argStrs
                    return ("(" <> name <> "_t){" <> initStrs <> "}", IRTypeStruct name fields)

                -- Regular function call case
                (_, Just (retType, _)) -> do
                    let argStrs = map fst argVals
                    let callExpr = T.concat [name, "(", T.intercalate ", " argStrs, ")"]
                    return (callExpr, retType)

                -- Unknown function - maintain existing function environment
                _ -> do
                    traceM $ "Unknown function: " ++ T.unpack name
                    let argStrs = map fst argVals
                    let callExpr = T.concat [name, "(", T.intercalate ", " argStrs, ")"]
                    -- Return IRTypeAny since we don't know the return type
                    return (callExpr, IRTypeAny)
        IRCall name args -> do
            traceM $ "Generating function call: " ++ T.unpack name
            state <- get
            argVals <- mapM generateTypedExpr args
            case M.lookup name (varEnv state) of
                Just (IRTypeStruct sName fields) -> do
                    -- Use actual field names from struct definition
                    let fieldNames = map fst fields
                    let argStrs = map fst argVals
                    let initStrs = T.intercalate ", " $
                          zipWith (\f v -> f <> ": " <> v) fieldNames argStrs
                    return ("(" <> name <> "_t){" <> initStrs <> "}", IRTypeStruct name fields)
                Just _ -> throwError $ UnsupportedType IRTypeString
                Nothing -> do
                    traceM $ "Unknown function: " ++ T.unpack name
                    throwError $ UnsupportedType IRTypeString

        _ -> throwError $ UnsupportedType IRTypeString

generateStmt :: IRExpr -> Codegen Text
generateStmt irExpr = do
    traceM $ "Generating statement for expression: " ++ show irExpr
    let IRExpr meta exprNode = irExpr
    case exprNode of
        IRLetAlloc name val strat -> do
          state <- get
          case M.lookup name (varEnv state) of
            -- Variable already exists - generate assignment
            Just _ -> do
              (valExpr, _) <- generateTypedExpr val
              return $ T.concat ["    ", name, " = ", valExpr, ";\n"]
            -- New variable - generate declaration
            Nothing -> do
              (valExpr, valType) <- generateTypedExpr val
              typeStr <- irTypeToCType valType
              -- Add variable to environment when we create it
              modify $ \s -> s { varEnv = M.insert name valType (varEnv s) }
              return $ T.concat ["    ", typeStr, " ", name, " = ", valExpr, ";\n"]

        IRPrint printExpr -> do
            (val, typ) <- generateTypedExpr printExpr
            fmt <- getPrintfFormat typ
            case typ of
                IRTypeVec (IRVec3 _) ->
                    return $ T.concat ["    printf(\"", fmt, "\\n\", ",
                                     val, ".x, ", val, ".y, ", val, ".z);"]
                IRTypeVec (IRVec4 _) ->
                    return $ T.concat ["    printf(\"", fmt, "\\n\", ", val, ");"]
                _ ->
                    return $ T.concat ["    printf(\"", fmt, "\\n\", ", val, ");"]

        IRBlock "while" (cond:body:_) _ -> do
            -- Special case for while blocks
            (condExpr, condType) <- generateTypedExpr cond
            bodyStmt <- generateStmt body
            return $ T.concat [
                "    while (", condExpr, ") {\n",
                bodyStmt, "\n",
                "    }"]

        IRBlock name exprs mResult -> do
            -- General case for other blocks
            stmts <- mapM generateStmt exprs
            res <- case mResult of
                Just r -> generateStmt r
                Nothing -> return ""
            return $ T.unlines (stmts ++ [res])

        IRBlockAlloc name exprs mResult -> do
            stmts <- mapM generateStmt exprs
            res <- case mResult of
                Just r -> generateStmt r
                Nothing -> return ""
            return $ T.unlines stmts <> res

        IRBinOp _ _ _ -> do
            (val, _) <- generateTypedExpr irExpr  -- Pass whole IRExpr
            return $ "    " <> val <> ";"

        IRStructLit sName fields -> do
            traceM $ "Generating statement for struct literal: " ++ T.unpack sName
            let tmpName = T.pack ("tmp_" ++ T.unpack sName)
            fieldVals <- mapM (\(fn, fe) -> do
                             (fv, ft) <- generateTypedExpr fe
                             return (fn, fv, ft)) fields
            let structType = IRTypeStruct sName [(fn, ft) | (fn,_,ft) <- fieldVals]
            cType <- irTypeToCType structType
            stmts <- forM fieldVals $ \(fn,fv,_) ->
                return $ T.concat ["    ", tmpName, ".", fn, " = ", fv, ";"]
            return $ T.concat ["    ", cType, " ", tmpName, ";\n", T.unlines stmts]

        
        IRFieldAccess base f -> do
            (val, _) <- generateTypedExpr irExpr  -- Pass whole IRExpr
            return $ "    " <> val <> ";"

        IRVarAlloc name strat -> do
            traceM $ "Generating var alloc for " ++ T.unpack name
            return $ "    // var alloc for " <> name

        IRResult val -> do
            (valExpr, _) <- generateTypedExpr val
            return $ "    // result: " <> valExpr

        IRBreak label ->
            return $ "    break; // " <> label

        IRIf cond then_ else_ -> do
            (cVal, _) <- generateTypedExpr cond
            thenSt <- generateStmt then_
            elseSt <- generateStmt else_
            return $ T.concat ["    if (", cVal, ") {\n", thenSt, "\n} else {\n", elseSt, "\n}"]

        IRVar name -> do
            (val, _) <- generateTypedExpr irExpr  -- Pass whole IRExpr
            return $ "    // variable ref: " <> val

        IRNum {} -> do
            (val, _) <- generateTypedExpr irExpr  -- Pass whole IRExpr
            return $ "    // number: " <> val

        IRString s ->
            return $ "    // string: \"" <> s <> "\""

        IRBool b ->
            return $ "    // bool: " <> (if b then "true" else "false")

        IRVecAlloc _ _ _ ->
            return "    // vec alloc not handled yet"

        IRStructLitAlloc {} ->
            return "    // struct lit alloc not handled yet"

        IRArrayLit {} ->
            return "    // array literal not handled yet"

        IRIndex {} ->
            return "    // index not handled yet"

        _ -> do
            (val, _) <- generateTypedExpr irExpr  -- Pass whole IRExpr
            return $ "    " <> val <> ";"

generateFuncDef :: IRDecl -> Codegen Text
generateFuncDef (IRFunc name params retType body) = do
    traceM $ "\n--> Starting function generation for '" ++ T.unpack name ++ "'"
    modify $ \s -> s { funcEnv = M.insert name (retType, map snd params) (funcEnv s) }

    forM_ params $ \(pname, ptype) -> do
        traceM $ "Adding parameter to environment: " ++ T.unpack pname ++ " : " ++ show ptype
        modify $ \s -> s { varEnv = M.insert pname ptype (varEnv s) }

    retTypeStr <- irTypeToCType retType
    paramStrs <- forM params $ \(pname, ptype) -> do
        typeStr <- irTypeToCType ptype
        return $ T.concat [typeStr, " ", pname]

    -- Handle block vs expression return values
    bodyCode <- case expr body of
        IRBlockAlloc _ stmts mResult -> do
            -- Generate statements
            stmtCode <- T.concat <$> mapM generateStmt stmts
            -- Generate return value
            retVal <- case (stmts, mResult) of
                -- Use last statement as return if no explicit result
                (expr:_, Nothing) -> do
                    (val, _) <- generateTypedExpr expr
                    return val
                -- Use explicit result expression if provided
                (_, Just resultExpr) -> do
                    (val, _) <- generateTypedExpr resultExpr
                    return val
                -- Default to 0 for empty block
                ([], Nothing) -> return "0"
            return $ stmtCode <> "    return " <> retVal <> ";\n"
        _ -> do
            (bodyExpr, _) <- generateTypedExpr body
            return $ "    return " <> bodyExpr <> ";\n"

    oldState <- get
    modify $ \s -> s { varEnv = M.empty, funcEnv = M.empty }

    let funcSig = T.concat [retTypeStr, " ", name, "(", T.intercalate ", " paramStrs, ")"]
    let result = T.unlines
            [ funcSig <> " {"
            , bodyCode
            , "}"
            ]

    put oldState
    return result
generateFuncDef _ = throwError $ UnsupportedType IRTypeString

generateStructDef :: IRDecl -> Codegen Text
generateStructDef (IRStruct name fields) = do
    fieldDefs <- mapM generateField fields
    let structName = name <> T.pack "_t"
    modify $ \s -> s { structDefs = structName : structDefs s,
                      varEnv = M.insert name (IRTypeStruct name fields) (varEnv s) }


    -- Generate constructor function parameters with proper type conversion
    fieldTypes <- mapM (\(fname, ftype) -> do
        typeName <- irTypeToCType ftype
        return (fname, typeName)) fields

    let paramList = T.intercalate ", " [typ <> " " <> fname | (fname, typ) <- fieldTypes]
    let assignments = T.intercalate ", " [fname <> " = " <> fname | (fname, _) <- fields]
    let constructor = T.unlines
          [ structName <> " new_" <> name <> "(" <> paramList <> ") {"
          , "    return (" <> structName <> "){" <> assignments <> "};"
          , "}"
          ]

    -- Join fields with newlines and proper indentation
    let fieldSection = T.concat
          [T.pack "    " <> f | f <- fieldDefs]

    return $ T.concat
        [ T.pack "typedef struct " <> name <> T.pack " {\n"
        , fieldSection
        , T.pack "} " <> structName <> T.pack ";\n"
        ]

generateField :: (Text, IRType) -> Codegen Text
generateField (fname, ftyp) = do
    cType <- irTypeToCType ftyp
    return $ cType <> T.pack " " <> fname <> T.pack ";\n"

irTypeToCType :: IRType -> Codegen Text
irTypeToCType (IRTypeNum nt) = return $ case nt of
  IRInt32 -> T.pack "int32_t"
  IRInt64 -> T.pack "int64_t"
  IRFloat32 -> T.pack "float"
  IRFloat64 -> T.pack "double"
irTypeToCType IRTypeString = return (T.pack "const char*")
irTypeToCType IRTypeBool = return (T.pack "_Bool")
irTypeToCType (IRTypeVec vt) = case vt of
  IRVec2 IRFloat32 -> return $ T.pack "v2f32"
  IRVec3 IRFloat32 -> return $ T.pack "v3f32"
  IRVec4 IRFloat32 -> return $ T.pack "v4f32"
  IRVec2 IRFloat64 -> return $ T.pack "v2f64"
  IRVec3 IRFloat64 -> return $ T.pack "v3f64"
  IRVec4 IRFloat64 -> return $ T.pack "v4f64"
  _ -> throwError $ UnsupportedType (IRTypeVec vt)
irTypeToCType (IRTypeStruct name _) = return (name <> T.pack "_t")
irTypeToCType (IRTypeArray t) = do
  baseType <- irTypeToCType t
  return $ baseType <> T.pack "*"
irTypeToCType _ = throwError $ UnsupportedType IRTypeString

getPrintfFormat :: IRType -> Codegen Text
getPrintfFormat typ = do
    traceM $ "Getting printf format for type: " ++ show typ
    case typ of
        IRTypeString -> return "%s"
        IRTypeNum nt -> case nt of
            IRInt32 -> return "%d"
            IRInt64 -> return "%ld"
            IRFloat32 -> return "%f"
            IRFloat64 -> return "%lf"
        IRTypeBool -> return "%d"
        IRTypeVec (IRVec3 _) -> return "(%f, %f, %f)"
        IRTypeVec (IRVec4 _) -> return "(%f, %f, %f, %f)"
        IRTypeVec _ -> return "(unsupported vector)"
        IRTypeStruct sName fields ->
            -- Just fallback on printing one field if needed
            return "%f"
        _ -> throwError $ UnsupportedType typ
