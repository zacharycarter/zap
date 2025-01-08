{-# LANGUAGE OverloadedStrings #-}
module Zap.Compiler
  ( compile'
  , CompileError(..)
  , CompileResult(..)
  , CompileOptions(..)
  , defaultCompileOptions
  , CompileStage(..)
  ) where

import qualified Data.Text as T

import Zap.AST (Program(..), TopLevel(..))
import Zap.Parser.Program (parseProgram)
import Zap.Parser.Core (ParseError(..))
import Zap.Analysis.Semantic (analyze, SemanticError)
import Zap.IR
import Zap.Codegen.C (generateC, CGenError)
import Zap.Analysis.Lexical (tokenize, Located, LexError)

data CompileStage
  = Lexing
  | Parsing
  | SemanticAnalysis
  | IRConversion
  | CodeGeneration
  deriving (Show, Eq, Ord)

data CompileOptions = CompileOptions
  { targetStage :: CompileStage
  , optimizationLevel :: Int
  , debugInfo :: Bool
  , outputPath :: Maybe FilePath
  }

defaultCompileOptions :: CompileOptions
defaultCompileOptions = CompileOptions
  { targetStage = CodeGeneration
  , optimizationLevel = 0
  , debugInfo = False
  , outputPath = Nothing
  }

data CompileError
  = LexicalError LexError
  | ParserError ParseError
  | AnalysisError SemanticError
  | IRConversionError IRConversionError
  | GenerationError CGenError
  deriving (Show, Eq)

data CompileResult = CompileResult
  { tokens :: Maybe [Located]
  , parsedAST :: Maybe [TopLevel]
  , program :: Maybe Program
  , analyzed :: Maybe Program
  , ir :: Maybe IRProgram
  , generatedCode :: Maybe T.Text
  } deriving (Show, Eq)

emptyResult :: CompileResult
emptyResult = CompileResult Nothing Nothing Nothing Nothing Nothing Nothing

-- | Compile Zap program using new IR
compile' :: CompileOptions -> T.Text -> Either CompileError CompileResult
compile' opts source = do
  -- Lexical analysis
  lexResult <- if targetStage opts >= Lexing
    then do
      toks <- mapLeft LexicalError $ tokenize source
      return $ emptyResult { tokens = Just toks }
    else return emptyResult

  -- Parsing
  parseResult <- if targetStage opts >= Parsing
    then do
      parsed <- mapLeft ParserError $ parseProgram source
      return $ lexResult { parsedAST = Just parsed }
    else return lexResult

  -- Create Program from parsed AST
  astResult <- if targetStage opts >= Parsing
    then case parsedAST parseResult of
      Just topLevels -> do
        let prog = Program topLevels
        return $ parseResult { program = Just prog }
      Nothing -> return parseResult
    else return parseResult

  -- Semantic Analysis
  semanticResult <- if targetStage opts >= SemanticAnalysis
    then case program astResult of
      Just prog -> do
        a <- mapLeft AnalysisError $ analyze prog
        return $ astResult { analyzed = Just a }
      Nothing -> return astResult
    else return astResult

  -- IR Conversion
  irResult <- if targetStage opts >= IRConversion
    then case analyzed semanticResult of
      Just prog -> do
        ir <- mapLeft IRConversionError $ convertToIR' prog
        return $ semanticResult { ir = Just ir }
      Nothing -> return semanticResult
    else return semanticResult

  -- Code Generation
  if targetStage opts >= CodeGeneration
      then case ir irResult of
          Just ir -> do
              code <- mapLeft GenerationError $ generateC ir
              return $ irResult { generatedCode = Just code }
          Nothing -> return irResult
      else return irResult

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
