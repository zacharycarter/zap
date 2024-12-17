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
import Zap.Codegen.C (generateC, CGenError)
import Zap.Analysis.Lexical (tokenize, Token, Located, LexError)
import Zap.Parser.Translate (translateToAST, TranslateError)

data CompileStage
  = Lexing
  | Parsing
  | SemanticAnalysis
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
  | TranslationError TranslateError
  | AnalysisError SemanticError
  | IRConversionError IRConversionError
  | GenerationError CGenError
  deriving (Show, Eq)

data CompileResult = CompileResult
  { tokens :: Maybe [Located]
  , parsedTokens :: Maybe (Located, Located)
  , program :: Maybe Program
  , analyzed :: Maybe Program
  , generatedCode :: Maybe T.Text
  } deriving (Show, Eq)

emptyResult :: CompileResult
emptyResult = CompileResult Nothing Nothing Nothing Nothing Nothing

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
      return $ lexResult { parsedTokens = Just parsed }
    else return lexResult

  -- AST Translation
  astResult <- if targetStage opts >= Parsing
    then case parsedTokens parseResult of
      Just (printTok, strTok) -> do
        let translationResult = runExcept $ translateToAST printTok strTok
        ast <- mapLeft TranslationError translationResult
        let prog = Program [TLExpr ast]
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

  -- Code Generation
  if targetStage opts >= CodeGeneration
    then case analyzed semanticResult of
      Just prog -> do
        ir <- mapLeft IRConversionError $ convertToIR prog
        code <- mapLeft GenerationError $ generateC ir
        return $ semanticResult { generatedCode = Just code }
      Nothing -> return semanticResult
    else return semanticResult

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
