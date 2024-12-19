{-# LANGUAGE OverloadedStrings #-}
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
  { varEnv :: M.Map T.Text IRType            -- For variables
  , funcEnv :: M.Map T.Text (IRType, [IRType]) -- (return type, parameter types)
  , blockCounter :: Int
  , structDefs :: [Text]
  }

generateC :: IR -> Either CGenError Text
generateC (IRProgram decls exprs) = evalState (runExceptT $ do
    traceM "\n=== Starting Complete Program Generation ==="

    -- Process declarations
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

    -- Process main function expressions
    traceM "\n--- Processing Main Function Expressions ---"
    mainBody <- forM exprs $ \expr -> do
        traceM $ "\nGenerating statement for:\n" ++ show expr
        res <- generateStmt expr
        traceM $ "Generated statement:\n" ++ T.unpack res
        return res

    -- Build complete program
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
          , T.pack "#include <immintrin.h>"
          , T.pack ""
          , vectorOps  -- Add the vector operations here
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
generateTypedExpr expr = do
    traceM $ "Generating expression: " ++ show expr
    case expr of
        IRNum t val -> do
            return (val, IRTypeNum t)

        IRString s ->
            return (T.concat ["\"", s, "\""], IRTypeString)

        IRVar name -> do
            varType <- gets (M.lookup name . varEnv)
            case varType of
                Just t -> return (name, t)
                Nothing -> throwError $ UnsupportedType IRTypeString

        IRVec (IRVec3 IRFloat32) components -> do
            componentVals <- mapM generateTypedExpr components
            let vals = map fst componentVals
            return (T.concat ["(v3f32){", T.intercalate ", " vals, "}"],
                    IRTypeVec (IRVec3 IRFloat32))

        IRBinOp IRAdd lhs rhs -> do
            (lval, ltyp) <- generateTypedExpr lhs
            (rval, rtyp) <- generateTypedExpr rhs
            case (ltyp, rtyp) of
                (IRTypeVec (IRVec3 IRFloat32), IRTypeVec (IRVec3 IRFloat32)) ->
                    return (T.concat ["vec3_add(", lval, ", ", rval, ")"],
                            IRTypeVec (IRVec3 IRFloat32))
                _ -> throwError $ UnsupportedOperation IRAdd ltyp rtyp

        IRBinOp op lhs rhs -> do
            (lval, ltyp) <- generateTypedExpr lhs
            (rval, rtyp) <- generateTypedExpr rhs
            case (op, ltyp, rtyp) of
                -- SIMD vector operations
                (IRAdd, IRTypeVec (IRVec4 IRFloat32), IRTypeVec (IRVec4 IRFloat32)) ->
                    return (T.concat ["_mm_add_ps(", lval, ", ", rval, ")"], ltyp)

                (IRDot, IRTypeVec (IRVec4 IRFloat32), IRTypeVec (IRVec4 IRFloat32)) ->
                    return (T.concat ["_mm_dp_ps(", lval, ", ", rval, ", 0xFF)"], IRTypeNum IRFloat32)

                -- Vec3 operations
                (IRAdd, IRTypeVec (IRVec3 IRFloat32), IRTypeVec (IRVec3 IRFloat32)) ->
                    return (T.concat ["vec3_add(", lval, ", ", rval, ")"],
                            IRTypeVec (IRVec3 IRFloat32))

                -- Numeric operations
                (IRAdd, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " + ", rval, ")"], IRTypeNum t1)

                (IRSub, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " - ", rval, ")"], IRTypeNum t1)

                (IRMul, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " * ", rval, ")"], IRTypeNum t1)

                (IRDiv, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
                    return (T.concat ["(", lval, " / ", rval, ")"], IRTypeNum t1)

                _ -> throwError $ UnsupportedOperation op ltyp rtyp

        IRCall name args -> do
            traceM $ "Generating function call: " ++ show name
            state <- get
            -- Generate argument expressions
            argVals <- mapM generateTypedExpr args
            let argStrs = map fst argVals
                argTypes = map snd argVals

            -- Look up function signature
            (returnType, paramTypes) <- case M.lookup name (funcEnv state) of
                Just sig -> return sig
                Nothing -> do
                    traceM $ "Function " ++ show name ++ " not found in environment"
                    throwError $ UnsupportedOperation IRAdd IRTypeString IRTypeString

            -- Verify argument types match parameters
            when (length argTypes /= length paramTypes) $
                throwError $ UnsupportedOperation IRAdd IRTypeString IRTypeString

            forM_ (zip argTypes paramTypes) $ \(argType, paramType) ->
                when (argType /= paramType) $
                    throwError $ UnsupportedOperation IRAdd argType paramType

            return (T.concat [name, "(", T.intercalate ", " argStrs, ")"],
                    returnType)

        IRFieldAccess vec field -> do
            traceM $ "Generating SIMD field access for field: " ++ show field
            (vecVal, vecType) <- generateTypedExpr vec
            case vecType of
                IRTypeVec vt -> do
                    let componentIndex = case field of
                            "x" -> "0"
                            "y" -> "1"
                            "z" -> "2"
                            "w" -> "3"
                            _ -> error $ "Invalid vector component: " ++ T.unpack field

                    let baseType = case vt of
                            IRVec2 t -> t
                            IRVec3 t -> t
                            IRVec4 t -> t

                    -- Generate SIMD extraction using _mm_extract_ps
                    return (T.concat ["_mm_extract_ps(", vecVal, ", ", componentIndex, ")"],
                           IRTypeNum baseType)

                _ -> throwError $ UnsupportedOperation IRAdd vecType vecType

getPrintfFormat :: IRType -> Codegen Text
getPrintfFormat typ = do
    traceM $ "Getting printf format for type: " ++ show typ
    case typ of
        IRTypeString -> do
            traceM "Using string format specifier"
            return "%s"
        IRTypeNum nt -> do
            traceM $ "Using numeric format specifier for " ++ show nt
            case nt of
                IRInt32 -> return "%d"
                IRInt64 -> return "%ld"
                IRFloat32 -> return "%f"
                IRFloat64 -> return "%lf"
        IRTypeBool -> do
            traceM "Using boolean format specifier"
            return "%d"
        IRTypeVec _ -> do
            traceM "Using vector format specifier"
            return "(%f, %f, %f)"
        IRTypeVec (IRVec4 _) -> do
            traceM "Using Vec4 format specifier"
            return "(%f, %f, %f, %f)"
        _ -> do
            traceM $ "Unsupported type for printing: " ++ show typ
            throwError $ UnsupportedType typ

generateStmt :: IRExpr -> Codegen Text
generateStmt expr = do
    traceM $ "Generating statement for expression: " ++ show expr
    case expr of
        IRLetAlloc name val strat -> do
            traceM $ "Generating let allocation for: " ++ show name
            (valExpr, valType) <- generateTypedExpr val
            typeStr <- irTypeToCType valType
            modify $ \s -> s { varEnv = M.insert name valType (varEnv s) }
            return $ T.concat ["    ", typeStr, " ", name, " = ", valExpr, ";"]

        IRPrint printExpr -> do
            traceM $ "Generating print statement for expression: " ++ show printExpr
            (val, typ) <- generateTypedExpr printExpr
            traceM $ "Print value generated: " ++ T.unpack val
            traceM $ "Print value type: " ++ show typ
            fmt <- getPrintfFormat typ
            case typ of
                IRTypeVec (IRVec3 _) -> do
                    traceM "Generating Vec3 print statement"
                    return $ T.concat ["    printf(\"", fmt, "\\n\", ",
                                     val, ".x, ", val, ".y, ", val, ".z);"]
                IRTypeVec (IRVec4 _) -> do
                    traceM "Generating Vec4 print statement"
                    return $ T.concat ["    printf(\"", fmt, "\\n\", ",
                                     val, ".x, ", val, ".y, ", val, ".z, ", val, ".w);"]
                _ -> do
                    traceM "Generating standard print statement"
                    return $ T.concat ["    printf(\"", fmt, "\\n\", ", val, ");"]

        IRBlockAlloc name exprs mResult -> do
            traceM $ "Generating block allocation: " ++ show name
            stmts <- mapM generateStmt exprs
            return $ T.unlines stmts

        IRBinOp op lhs rhs -> do
            traceM $ "Generating binary operation statement"
            (val, typ) <- generateTypedExpr expr
            return $ T.concat ["    ", val, ";"]

        _ -> do
            traceM $ "Generating statement for other expression type: " ++ show expr
            (val, _) <- generateTypedExpr expr
            return $ T.concat ["    ", val, ";"]

generateFuncDef :: IRDecl -> Codegen Text
generateFuncDef (IRFunc name params retType body) = do
    traceM $ "\n--> Starting function generation for '" ++ T.unpack name ++ "'"

    -- Store function signature in environment before processing body
    modify $ \s -> s { funcEnv = M.insert name (retType, map snd params) (funcEnv s) }

    -- Add parameters to variable environment with proper types
    forM_ params $ \(pname, ptype) -> do
        traceM $ "Adding parameter to environment: " ++ T.unpack pname ++ " : " ++ show ptype
        modify $ \s -> s { varEnv = M.insert pname ptype (varEnv s) }

    -- Generate return type and parameters
    retTypeStr <- irTypeToCType retType
    paramStrs <- forM params $ \(pname, ptype) -> do
        typeStr <- irTypeToCType ptype
        return $ T.concat [typeStr, " ", pname]

    -- Generate function body
    (bodyExpr, bodyType) <- generateTypedExpr body

    -- Save current environment state
    oldState <- get

    -- Clear environments to prevent leaking
    modify $ \s -> s { varEnv = M.empty, funcEnv = M.empty }

    -- Generate final function text
    let funcSig = T.concat [retTypeStr, " ", name, "(", T.intercalate ", " paramStrs, ")"]
    let result = T.unlines
            [ funcSig <> " {"
            , "    return " <> bodyExpr <> ";"
            , "}"
            ]

    -- Restore previous environment
    put oldState

    return result

generateFuncDef _ = throwError $ UnsupportedType IRTypeString

generateStructDef :: IRDecl -> Codegen Text
generateStructDef (IRStruct name fields) = do
  fieldDefs <- mapM generateField fields
  let structName = name <> T.pack "_t"
  modify $ \s -> s { structDefs = structName : structDefs s }
  return $ T.concat
    [ T.pack "typedef struct " <> name <> T.pack " {\n"
    , T.concat (map (\f -> T.pack "    " <> f <> T.pack "\n") fieldDefs)
    , T.pack "} " <> structName <> T.pack ";\n"
    ]
generateStructDef _ = throwError $ UnsupportedType IRTypeString

generateField :: (Text, IRType) -> Codegen Text
generateField (name, typ) = do
  cType <- irTypeToCType typ
  return $ cType <> T.pack " " <> name <> T.pack ";"

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
irTypeToCType (IRTypeStruct name _) = return $ name <> T.pack "_t"
irTypeToCType (IRTypeArray t) = do
  baseType <- irTypeToCType t
  return $ baseType <> T.pack "*"
irTypeToCType _ = throwError $ UnsupportedType IRTypeString
