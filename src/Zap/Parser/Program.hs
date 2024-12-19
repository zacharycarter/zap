{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Program
  ( parseProgram
  , parseTopLevel
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T
import Debug.Trace
import Zap.Analysis.Lexical
import Zap.Parser.Types
import Zap.Parser.Core
import Zap.Parser.Expr
import Zap.AST

parseProgram :: T.Text -> Either ParseError [TopLevel]
parseProgram input = do
    traceM "\n=== Starting Program Parsing ==="
    traceM $ "Input text: " ++ T.unpack input
    tokens <- mapLexError $ tokenize input
    traceM $ "Tokenized input: " ++ show tokens
    runParser parseTopLevels tokens

parseTopLevels :: Parser [TopLevel]
parseTopLevels = do
    state <- get
    traceM "\n--- Parsing Top Level Expressions ---"
    traceM $ "Current tokens: " ++ show (take 3 $ stateTokens state)
    case stateTokens state of
        [] -> do
            traceM "No more tokens to parse"
            pure []
        (tok:_) -> case locToken tok of
            TEOF -> do
                traceM "Found EOF token"
                pure []
            _ -> do
                traceM $ "Parsing top-level expression starting with: " ++ show tok
                expr <- parseTopLevel
                traceM $ "Successfully parsed expression: " ++ show expr
                rest <- parseTopLevels
                pure (expr : rest)

parseTopLevel :: Parser TopLevel
parseTopLevel = do
    state <- get
    traceM "\n--- Parsing Single Top Level Expression ---"
    traceM $ "Current state: " ++ show state
    case stateTokens state of
        (tok:_) -> do
            traceM $ "Processing token: " ++ show tok
            case locToken tok of
                TType -> do
                    traceM "Found type definition"
                    _ <- matchToken (== TType) "type"
                    typeName <- matchToken isValidName "type name"
                    _ <- matchToken (== TEquals) "equals sign"
                    typeDefinition <- parseTypeDefinition
                    case locToken typeName of
                        TWord name -> return $ TLType name typeDefinition
                        _ -> throwError $ UnexpectedToken typeName "type name"
                TWord "print" -> do
                    traceM "Found print statement"
                    expr <- parsePrintStatement
                    return $ TLExpr expr
                TWord "block" -> do
                    traceM "Found block expression"
                    when (locCol tok /= 1) $
                        throwError $ IndentationError 1 (locCol tok) Equal
                    expr <- parseBlock defaultExprParser
                    return $ TLExpr expr
                TWord "let" -> do
                    traceM "Found let binding"
                    expr <- parseLetBinding defaultExprParser
                    return $ TLExpr expr
                _ -> do
                    traceM $ "Unexpected token in top level: " ++ show tok
                    throwError $ UnexpectedToken tok "expected 'print', 'let', or 'block'"
        [] -> do
            traceM "No tokens available for top-level expression"
            throwError $ EndOfInput "expected top-level expression"

parseTypeDefinition :: Parser Type
parseTypeDefinition = do
    traceM "Parsing type definition"
    _ <- matchToken (== TStruct) "struct"
    fields <- parseStructFields
    return $ TypeStruct "" fields

parseStructFields :: Parser [(String, Type)]
parseStructFields = do
    traceM "Parsing struct fields"
    let loop acc = do
          state <- get
          case stateTokens state of
            (tok:_) | isValidName (locToken tok) -> do
                        field <- parseStructField
                        loop (field : acc)
            _ -> return $ reverse acc
    loop []

parseStructField :: Parser (String, Type)
parseStructField = do
    traceM "Parsing struct field"
    name <- matchToken isValidName "field name"
    _ <- matchToken (== TColon) "colon"
    fieldType <- parseType
    case locToken name of
        TWord fieldName -> return (fieldName, fieldType)
        _ -> throwError $ UnexpectedToken name "field name"

parseType :: Parser Type
parseType = do
    traceM "Parsing type"
    tok <- matchToken isValidName "type name"
    case locToken tok of
        TWord "Float32" -> return $ TypeNum Float32
        TWord "Float64" -> return $ TypeNum Float64
        TWord "Int32" -> return $ TypeNum Int32
        TWord "Int64" -> return $ TypeNum Int64
        _ -> throwError $ UnexpectedToken tok "type name"

parsePrintStatement :: Parser Expr
parsePrintStatement = do
    state <- get
    traceM "Parsing print statement"
    expectedIndent <- gets stateIndent
    let tok = head $ stateTokens state
    when (locCol tok < expectedIndent) $
        throwError $ IndentationError expectedIndent (locCol tok) GreaterEq
    _ <- matchToken isPrint "print"
    expr <- parseExpression  -- Change from parseBasicExpr to parseExpression
    return $ Print expr

mapLexError :: Either LexError a -> Either ParseError a
mapLexError (Left (UnterminatedString line col)) =
    Left $ EndOfInput $ "Unterminated string at line " ++ show line ++ ", column " ++ show col
mapLexError (Left (InvalidCharacter c line col)) =
    Left $ EndOfInput $ "Invalid character '" ++ [c] ++ "' at line " ++ show line ++ ", column " ++ show col
mapLexError (Right a) = Right a
