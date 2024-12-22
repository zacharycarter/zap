{-# LANGUAGE OverloadedStrings #-}
module Zap.Compiler
  ( compile
  , CompileError(..)
  , CompileResult(..)
  , CompileOptions(..)
  , defaultCompileOptions
  , CompileStage(..)
  ) where

import qualified Data.Text as T
import Control.Monad.Except

import Zap.AST (Program(..), Expr(..), TopLevel(..))
import Zap.Parser.Program (parseProgram)
import Zap.Parser.Core (ParseError(..))
import Zap.Analysis.Semantic (analyze, SemanticError)
import Zap.IR.Conversion
import Zap.IR.Core (IR)
import Zap.Analysis.AllocationOpt (optimizeAllocations, OptimizationStats(..))
import Zap.Codegen.C (generateC, CGenError)
import Zap.Analysis.Lexical (tokenize, Token, Located, LexError)

data CompileStage
  = Lexing
  | Parsing
  | SemanticAnalysis
  | IROptimization
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
  | OptimizationError String
  | GenerationError CGenError
  deriving (Show, Eq)

data CompileResult = CompileResult
  { tokens :: Maybe [Located]
  , parsedAST :: Maybe [TopLevel]
  , program :: Maybe Program
  , analyzed :: Maybe Program
  , optimizationStats :: Maybe OptimizationStats
  , generatedCode :: Maybe T.Text
  } deriving (Show, Eq)

emptyResult :: CompileResult
emptyResult = CompileResult Nothing Nothing Nothing Nothing Nothing Nothing

compile :: CompileOptions -> T.Text -> Either CompileError CompileResult
compile opts source = do
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
        analyzed <- mapLeft AnalysisError $ analyze prog
        return $ astResult { analyzed = Just analyzed }
      Nothing -> return astResult
    else return astResult

  -- IR Generation and Optimization
  irOptResult <- if targetStage opts >= IROptimization
    then case analyzed semanticResult of
      Just prog -> do
        ir <- mapLeft IRConversionError $ convertToIR prog
        case optimizeAllocations ir of
          Right (optimizedIr, stats) ->
            return $ semanticResult { optimizationStats = Just stats }
          Left err -> Left $ OptimizationError (show err)
      Nothing -> return semanticResult
    else return semanticResult

  -- Code Generation
  if targetStage opts >= CodeGeneration
    then case analyzed semanticResult of
      Just prog -> do
        ir <- mapLeft IRConversionError $ convertToIR prog
        -- Apply optimizations if enabled
        finalIr <- if optimizationLevel opts > 0
          then case optimizeAllocations ir of
            Right (optimized, _) -> Right optimized
            Left err -> Left $ OptimizationError (show err)
          else Right ir
        code <- mapLeft GenerationError $ generateC finalIr
        return $ irOptResult { generatedCode = Just code }
      Nothing -> return irOptResult
    else return irOptResult

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
