{-# LANGUAGE OverloadedStrings #-}
module Zap.Codegen.C
  ( generateC
  , CGenError(..)
  ) where

import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Debug.Trace

import Zap.IR
import Zap.AST (Op(..))

data CGenError
  = UnsupportedType IRType
  | UnsupportedOperation T.Text
  deriving (Show, Eq)

generateC :: IRProgram -> Either CGenError T.Text
generateC (IRProgram funcs) = do
    traceM "\n=== Starting C Code Generation ==="

    -- Generate each function and combine results
    functionDefs <- mapM generateFunction funcs

    let program = T.unlines
          [ "#include <stdio.h>"
          , "#include <stdint.h>"
          , ""
          , T.unlines functionDefs  -- functionDefs is already [Text]
          ]

    traceM $ "\n=== Final Generated Program ===\n" ++ T.unpack program

    -- Combine headers and function definitions
    return program

generateFunction :: (IRFuncDecl, IRMetadata) -> Either CGenError T.Text
generateFunction (func, _) = do
    body <- generateBlock (fnBody func)

    -- Determine return type for function signature
    let typeStr = case fnRetType func of
          IRTypeInt -> "int"
          IRTypeInt32 -> "int32_t"
          IRTypeVoid -> "void"
          _ -> "int"  -- Default to int for now

    -- Generate function signature based on name and parameters
    let signature = case fnName func of
          "main" -> T.pack "int main(void)"  -- Preserve existing main() handling
          _ -> T.concat [T.pack typeStr, " ", T.pack (fnName func), "(", generateParams (fnParams func), ")"]

    return $ T.unlines
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
    paramTypeToC IRTypeInt = "int"
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
        return $ T.concat ["    int ", T.pack name, " = ", exprCode, ";"]
    IRStmtExpr expr -> generateExpr expr
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

    IRJumpIfZero cond label -> do
        condCode <- generateExpr cond
        return $ T.concat ["    if (!(", condCode, ")) goto ", T.pack label, ";"]

    IRProcCall "print" [expr] -> do
        (value, fmt) <- generatePrintExpr expr
        return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]
    IRProcCall name _ ->
        Left $ UnsupportedOperation $ T.pack $ "Unsupported procedure: " ++ name

-- Helper for generating literal values
generateLiteral :: IRLiteral -> T.Text
generateLiteral (IRIntLit n) = T.pack (show n)
generateLiteral (IRVarRef name) = T.pack name
generateLiteral (IRStringLit s) = T.concat ["\"", T.pack s, "\""]

generateExpr :: IRExpr -> Either CGenError T.Text
generateExpr (IRCall "print" [expr]) = do
    (value, fmt) <- generatePrintExpr expr
    return $ T.concat ["    printf(\"", fmt, "\\n\", ", value, ");"]

generateExpr (IRCall op [e1, e2]) = do
    -- Map operator strings to C operators
    let cOp = case op of
          "Add" -> "+"
          "Sub" -> "-"
          "Mul" -> "*"
          "Div" -> "/"
          "Lt"  -> "<"
          "Gt"  -> ">"
          "Eq"  -> "=="
          _ -> error $ "Unsupported operator: " ++ op

    -- Generate code for operands
    left <- generateExpr e1
    right <- generateExpr e2
    return $ T.concat ["(", left, ") ", T.pack cOp, " (", right, ")"]

generateExpr (IRLit lit) = return $ generateLiteral lit

generateExpr (IRVar name) = return $ T.pack name

generateExpr expr = Left $ UnsupportedOperation $ T.pack $ "Unsupported expression: " ++ show expr

-- Helper to determine format specifier
generatePrintExpr :: IRExpr -> Either CGenError (T.Text, T.Text)
generatePrintExpr (IRLit (IRStringLit str)) =
    Right (T.concat ["\"", T.pack str, "\""], "%s")
generatePrintExpr (IRLit (IRIntLit n)) =
    Right (T.pack (show n), "%d")
generatePrintExpr (IRLit (IRVarRef name)) =
    Right (T.pack name, "%d")  -- Assuming variables are ints for now
generatePrintExpr (IRCall fname args) = do
    -- For function calls, generate the call and use int format
    argCode <- mapM generateExpr args
    let call = T.concat [T.pack fname, "(", T.intercalate ", " argCode, ")"]
    Right (call, "%d")
generatePrintExpr expr =
    Left $ UnsupportedOperation $ T.pack $ "Unsupported print expression: " <> show expr
