{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Program
  ( parseProgram
  , parseTopLevel
  ) where

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T
import Debug.Trace
import Zap.Analysis.Lexical
import Zap.Parser.Types
import Zap.Parser.Core
import Zap.Parser.Expr (isValidName, parsePrintStatement, parseBlock, defaultExprParser, parseLetBinding, parseSingleBindingLine)
import Zap.AST

parseProgram :: T.Text -> Either ParseError [TopLevel]
parseProgram input = do
    traceM "\n=== Starting Program Parsing ==="
    traceM $ "Input text: " ++ T.unpack input
    tokens <- mapLexError $ tokenize input
    traceM $ "Tokenized input: " ++ show tokens
    result <- runParser parseTopLevels tokens
    traceM $ "Parsed top-levels: " ++ show result
    return result

parseTopLevels :: Parser [TopLevel]
parseTopLevels = do
    state <- get
    traceM "\n--- Parsing Top Level Expressions ---"
    traceM $ "Current tokens: " ++ show (take 3 $ stateTokens state)
    case stateTokens state of
        [] -> do
            traceM "No more tokens to parse, returning empty list"
            pure []
        (tok:_) -> case locToken tok of
            TEOF -> do
                traceM "Found EOF token, returning empty list"
                pure []
            _ -> do
                traceM $ "Parsing top-level expression starting with: " ++ show tok
                expr <- parseTopLevel
                traceM $ "Successfully parsed top-level expression: " ++ show expr
                rest <- parseTopLevels
                traceM $ "Collected expressions so far: " ++ show (expr:rest)
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
                    typeNameTok <- matchToken isValidName "type name"
                    _ <- matchToken (== TEquals) "equals sign"
                    case locToken typeNameTok of
                        TWord name -> do
                            traceM $ "Parsing type definition for: " ++ name
                            typeDefinition <- parseTypeDefinition (T.pack name)
                            let tl = TLType name typeDefinition
                            traceM $ "Parsed type definition: " ++ show tl
                            return tl
                        _ -> throwError $ UnexpectedToken typeNameTok "type name"
                TWord "print" -> do
                    traceM "Found print statement at top-level"
                    expr <- parsePrintStatement
                    traceM $ "Parsed print statement: " ++ show expr
                    return $ TLExpr expr
                TWord "block" -> do
                    traceM "Found block expression at top-level"
                    when (locCol tok /= 1) $
                        throwError $ IndentationError 1 (locCol tok) Equal
                    expr <- parseBlock defaultExprParser
                    traceM $ "Parsed block: " ++ show expr
                    return $ TLExpr expr
                TWord "let" -> do
                    traceM "Found let block at top-level"
                    expr <- parseLetBlock
                    traceM $ "Parsed let block: " ++ show expr
                    return $ TLExpr expr
                _ -> do
                    traceM $ "Unexpected token in top level: " ++ show tok
                    throwError $ UnexpectedToken tok "expected 'print', 'let', or 'block'"
        [] -> do
            traceM "No tokens available for top-level expression"
            throwError $ EndOfInput "expected top-level expression"

parseLetBlock :: Parser Expr
parseLetBlock = do
    traceM "Parsing let block"
    _ <- matchToken (\t -> t == TWord "let") "let keyword"

    -- Get first binding and determine block indent level
    firstBinding <- do
        (name, expr) <- parseSingleBindingLine
        return $ Let name expr

    traceM $ "First binding parsed: " ++ show firstBinding

    -- Parse additional bindings at same indentation level
    let blockIndent = 2  -- standard indentation for let blocks
    moreBindings <- parseMoreBindings blockIndent
    traceM $ "Additional bindings parsed: " ++ show moreBindings

    let allBindings = firstBinding : map (\(n,v) -> Let n v) moreBindings
    let blockExpr = Block $ BlockScope
          { blockLabel = "top_let"
          , blockExprs = allBindings
          , blockResult = Nothing }
    traceM $ "Constructed let block expr: " ++ show blockExpr
    return blockExpr
  where
    parseMoreBindings :: Int -> Parser [(String, Expr)]
    parseMoreBindings indent = do
        state <- get
        traceM $ "Checking for more bindings at indent " ++ show indent ++ ": " ++ show (take 3 $ stateTokens state)
        case stateTokens state of
            [] -> return []
            (tok:_)
                | locToken tok == TEOF -> return []
                | locCol tok < indent -> do
                    -- Only end if we actually dedent
                    traceM $ "Let block ended due to dedent: " ++ show (locCol tok) ++ " < " ++ show indent
                    return []
                | locToken tok == TWord "print" && locCol tok == 1 -> do
                    -- End block when we see a non-indented print
                    traceM "Let block ended due to top-level print"
                    return []
                | locCol tok == indent || locCol tok > indent -> do
                    -- Continue parsing bindings at same or greater indentation
                    if isValidName (locToken tok)
                        then do
                            traceM "Found another binding line"
                            (n,v) <- parseSingleBindingLine
                            rest <- parseMoreBindings indent
                            return ((n,v):rest)
                        else do
                            traceM $ "No more bindings (token not a name): " ++ show tok
                            return []
                | otherwise -> do
                    traceM $ "Let block ended due to other token: " ++ show tok
                    return []

parseTypeDefinition :: T.Text -> Parser Type
parseTypeDefinition givenName = do
    traceM $ "Parsing type definition for " ++ T.unpack givenName
    _ <- matchToken (== TStruct) "struct"
    fields <- parseStructFields
    let t = TypeStruct (T.unpack givenName) fields
    traceM $ "Constructed TypeStruct: " ++ show t
    return t

parseStructFields :: Parser [(String, Type)]
parseStructFields = do
    traceM "Parsing struct fields"
    let loop acc = do
          state <- get
          traceM $ "Struct fields loop, tokens: " ++ show (take 3 $ stateTokens state)
          case stateTokens state of
            (tok:_) | isValidName (locToken tok) -> do
                        field <- parseStructField
                        traceM $ "Parsed struct field: " ++ show field
                        loop (field : acc)
            _ -> do
                traceM "No more struct fields"
                return $ reverse acc
    result <- loop []
    traceM $ "All struct fields parsed: " ++ show result
    return result

parseStructField :: Parser (String, Type)
parseStructField = do
    traceM "Parsing struct field"
    name <- matchToken isValidName "field name"
    _ <- matchToken (== TColon) "colon"
    fieldType <- parseType
    let fieldName = case locToken name of
          TWord fn -> fn
          _ -> error "Invalid field name token"
    traceM $ "Field parsed: " ++ fieldName ++ ": " ++ show fieldType
    return (fieldName, fieldType)

parseType :: Parser Type
parseType = do
    traceM "Parsing type"
    tok <- matchToken isValidName "type name"
    case locToken tok of
        TWord "f32" -> do
            traceM "Recognized Float32"
            return $ TypeNum Float32
        TWord "f64" -> do
            traceM "Recognized Float64"
            return $ TypeNum Float64
        TWord "i32"   -> do
            traceM "Recognized Int32"
            return $ TypeNum Int32
        TWord "i64"   -> do
            traceM "Recognized Int64"
            return $ TypeNum Int64
        TWord other     -> do
            traceM $ "Unrecognized type name: " ++ other
            throwError $ UnexpectedToken tok "type name"
        _ -> throwError $ UnexpectedToken tok "type name"

mapLexError :: Either LexError a -> Either ParseError a
mapLexError (Left (UnterminatedString line col)) =
    Left $ EndOfInput $ "Unterminated string at line " ++ show line ++ ", column " ++ show col
mapLexError (Left (InvalidCharacter c line col)) =
    Left $ EndOfInput $ "Invalid character '" ++ [c] ++ "' at line " ++ show line ++ ", column " ++ show col
mapLexError (Right a) = Right a
