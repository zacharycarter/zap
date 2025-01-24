{-# LANGUAGE OverloadedStrings #-}

module Zap.Compiler
  ( compile',
    CompileError (..),
    CompileResult (..),
    CompileOptions (..),
    defaultCompileOptions,
    CompileStage (..),
  )
where

import qualified Data.Text as T
import Zap.AST
import Zap.Analysis.Lexical (LexError, Located, tokenize)
import Zap.Analysis.Semantic (SemanticError, analyzeWithSymbols)
import Zap.Codegen.C (CGenError, generateC)
import Zap.IR
import Zap.Parser.Core (ParseError (..))
import Zap.Parser.Program (parseProgram)

data CompileStage
  = Lexing
  | Parsing
  | SemanticAnalysis
  | IRConversion
  | CodeGeneration
  deriving (Show, Eq, Ord)

data CompileOptions = CompileOptions
  { targetStage :: CompileStage,
    optimizationLevel :: Int,
    debugInfo :: Bool,
    outputPath :: Maybe FilePath
  }

defaultCompileOptions :: CompileOptions
defaultCompileOptions =
  CompileOptions
    { targetStage = CodeGeneration,
      optimizationLevel = 0,
      debugInfo = False,
      outputPath = Nothing
    }

data CompileError
  = LexicalError LexError
  | ParserError ParseError
  | AnalysisError SemanticError
  | IRConversionError IRConversionError
  | GenerationError CGenError
  deriving (Show, Eq)

data CompileResult = CompileResult
  { tokens :: Maybe [Located],
    parsedAST :: Maybe [TopLevel],
    symbolTable :: Maybe SymbolTable,
    program :: Maybe Program,
    analyzed :: Maybe Program,
    ir :: Maybe IRProgram,
    generatedCode :: Maybe T.Text
  }
  deriving (Show, Eq)

emptyResult :: CompileResult
emptyResult = CompileResult Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Compile Zap program using new IR
compile' :: CompileOptions -> T.Text -> Either CompileError CompileResult
compile' opts source = do
  -- Lexical analysis remains same
  lexResult <-
    if targetStage opts >= Lexing
      then do
        toks <- mapLeft LexicalError $ tokenize source
        return $ emptyResult {tokens = Just toks}
      else return emptyResult

  -- Parsing - now handling symbol table
  parseResult <-
    if targetStage opts >= Parsing
      then do
        (parsed, symTable) <- mapLeft ParserError $ parseProgram source
        return $
          lexResult
            { parsedAST = Just parsed,
              symbolTable = Just symTable
            }
      else return lexResult

  -- Create Program with symbol table
  astResult <-
    if targetStage opts >= Parsing
      then case (parsedAST parseResult, symbolTable parseResult) of
        (Just topLevels, Just _) -> do
          let prog = Program topLevels
          return $ parseResult {program = Just prog}
        _ -> return parseResult
      else return parseResult

  -- Semantic Analysis
  semanticResult <-
    if targetStage opts >= SemanticAnalysis
      then case (program astResult, symbolTable astResult) of
        (Just prog, Just symTable) -> do
          a <- mapLeft AnalysisError $ analyzeWithSymbols prog symTable
          return $ astResult {analyzed = Just a}
        _ -> return astResult
      else return astResult

  -- IR Conversion
  irResult <-
    if targetStage opts >= IRConversion
      then case (analyzed semanticResult, symbolTable semanticResult) of
        (Just prog, Just symTable) -> do
          ir' <- mapLeft IRConversionError $ convertToIR' prog symTable
          return $ semanticResult {ir = Just ir'}
        _ -> return semanticResult
      else return semanticResult

  -- Code Generation
  if targetStage opts >= CodeGeneration
    then case ir irResult of
      Just ir' -> do
        code <- mapLeft GenerationError $ generateC ir'
        return $ irResult {generatedCode = Just code}
      Nothing -> return irResult
    else return irResult

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
