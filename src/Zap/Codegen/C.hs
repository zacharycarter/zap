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

    let signature = case fnName func of
          "main" -> T.pack "int main(void)"
          _ -> error "Only main function supported currently"

    return $ T.unlines
      [ signature <> T.pack " {"
      , body
      , "    return 0;"
      , "}"
      ]

generateBlock :: IRBlock -> Either CGenError T.Text
generateBlock (IRBlock _ stmts) = do
    generatedStmts <- mapM generateStmt stmts
    return $ T.unlines generatedStmts

generateStmt :: (IRStmt, IRMetadata) -> Either CGenError T.Text
generateStmt (stmt, _) = case stmt of
    IRStmtExpr expr -> generateExpr expr
    IRReturn Nothing -> return "    return 0;"
    IRReturn (Just _) -> error "Return values not supported yet"

generateExpr :: IRExpr -> Either CGenError T.Text
generateExpr (IRCall "print" [IRStringLit str]) =
    return $ T.concat ["    printf(\"%s\\n\", \"", T.pack str, "\");"]
generateExpr (IRCall "print" [IRIntLit n]) =
    return $ T.concat ["    printf(\"%d\\n\", ", T.pack (show n), ");"]
generateExpr expr =
    Left $ UnsupportedOperation $ T.pack $ "Unsupported expression: " <> show expr
