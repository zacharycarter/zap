{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Program
  ( parseProgram
  , parseTopLevel
  ) where

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T
import Zap.Analysis.Lexical
import Zap.Parser.Types
import Zap.Parser.Core
import Zap.Parser.Expr
import Zap.AST

-- | Parse a complete program
parseProgram :: T.Text -> Either ParseError [TopLevel]
parseProgram input = do
  tokens <- mapLexError $ tokenize input
  runParser parseTopLevels tokens

-- | Parse multiple top-level expressions
parseTopLevels :: Parser [TopLevel]
parseTopLevels = do
  state <- get
  case stateTokens state of
    [] -> pure []
    (tok:_) -> case locToken tok of
      TEOF -> pure []
      _ -> do
        expr <- parseTopLevel
        rest <- parseTopLevels
        pure (expr : rest)

-- | Parse a single top-level expression
parseTopLevel :: Parser TopLevel
parseTopLevel = do
  state <- get
  case stateTokens state of
    (tok:_) -> case locToken tok of
      TWord "print" -> TLExpr <$> parsePrintStmt
      TWord "block" -> TLExpr <$> parseBlockExpr
      _ -> throwError $ UnexpectedToken tok "expected 'print' or 'block'"
    [] -> throwError $ EndOfInput "expected top-level expression"

-- | Parse a print statement
parsePrintStmt :: Parser Expr
parsePrintStmt = do
  state <- get
  when (locCol (head $ stateTokens state) /= 1) $
    throwError $ IndentationError 1 (locCol (head $ stateTokens state)) Equal
  printTok <- matchToken isPrint "print"
  modify (\s -> s { stateIndent = locCol printTok + 1 })
  strTok <- matchToken isStringLit "string literal"
  let printLine = locLine printTok
  when (locLine strTok > printLine && locCol strTok <= locCol printTok) $
    throwError $ IndentationError (locCol printTok + 1) (locCol strTok) Greater
  case locToken strTok of
    TString s -> pure $ Print (StrLit s)
    _ -> error "Matched non-string token as string literal"

-- | Parse a block expression
parseBlockExpr :: Parser Expr
parseBlockExpr = do
  state <- get
  when (locCol (head $ stateTokens state) /= 1) $
    throwError $ IndentationError 1 (locCol (head $ stateTokens state)) Equal
  expr <- parseExpr defaultExprParser
  case expr of
    Block {} -> pure expr
    _ -> throwError $ UnexpectedToken (head $ stateTokens state) "block"

mapLexError :: Either LexError a -> Either ParseError a
mapLexError (Left (UnterminatedString line col)) =
  Left $ EndOfInput $ "Unterminated string at line " ++ show line ++ ", column " ++ show col
mapLexError (Left (InvalidCharacter c line col)) =
  Left $ EndOfInput $ "Invalid character '" ++ [c] ++ "' at line " ++ show line ++ ", column " ++ show col
mapLexError (Right a) = Right a
