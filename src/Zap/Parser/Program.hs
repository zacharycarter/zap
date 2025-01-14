{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Program
  ( parseProgram
  , parseTopLevel
  , parseStructFields
  ) where

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace
import Zap.Analysis.Lexical
import Zap.Parser.Types
import Zap.Parser.Core
import Zap.Parser.Expr
import Zap.AST

parseProgram :: T.Text -> Either ParseError ([TopLevel], SymbolTable)
parseProgram input = do
    traceM "\n=== Starting Program Parsing ==="
    traceM $ "Input text: " ++ T.unpack input
    tokens <- mapLexError $ tokenize input
    traceM $ "Tokenized input: " ++ show tokens
    (result, finalState) <- runParserWithState parseTopLevels tokens
    traceM $ "Parsed top-levels: " ++ show result
    return (result, stateSymTable finalState)

runParserWithState :: Parser a -> [Located] -> Either ParseError (a, ParseState)
runParserWithState p tokens = runStateT p (ParseState tokens 0 0 emptySymbolTable)

parseTopLevels :: Parser [TopLevel]
parseTopLevels = do
  traceM "\n=== Starting parseTopLevels ==="
  let ctx = makeIndentContext TopLevel 0 0
  parseTopLevelsWith ctx

parseTopLevelsWith :: IndentContext -> Parser [TopLevel]
parseTopLevelsWith ctx = do
    st <- get
    traceM $ "\n=== parseTopLevelsWith ==="
    traceM $ "Context type: " ++ show ctx
    traceM $ "Current state indent: " ++ show (stateIndent st)
    traceM $ "Current context: " ++ show ctx
    traceM $ "Current tokens: " ++ show (take 3 $ stateTokens st)

    case stateTokens st of
        [] -> return []
        (tok:_) -> case locToken tok of
            TEOF -> do
                traceM "Encountered TEOF while parsing top level expressions"
                return []
            TType -> do
                let typeCtx = IndentContext { baseIndent = locCol tok + 2
                                           , parentIndent = baseIndent ctx
                                           , blockType = BasicBlock }
                traceM $ "Created type context: " ++ show typeCtx
                expr <- parseTopLevel ctx

                -- Look ahead at next token
                st' <- get
                case stateTokens st' of
                    (nextTok:_) | locToken nextTok == TWord "let" ||
                                 locToken nextTok == TWord "print" -> do
                        traceM $ "Found non-type declaration, reverting to top context"
                        modify $ \s -> s { stateIndent = 0 }  -- Reset indent
                        rest <- parseTopLevelsWith ctx
                        return (expr : rest)
                    _ -> do
                        -- Continue with type context for more type declarations
                        traceM $ "Continuing with type context: " ++ show typeCtx
                        rest <- parseTopLevelsWith typeCtx
                        return (expr : rest)
            _ -> do
                expr <- parseTopLevel ctx
                rest <- parseTopLevelsWith ctx
                traceM $ "Parsed top level: " ++ show expr ++ " " ++ show rest
                return (expr : rest)

 
parseTopLevel :: IndentContext -> Parser TopLevel
parseTopLevel ctx = do
    st <- get
    traceM "\n--- Parsing Single Top Level Expression ---"
    traceM $ "Current state indent: " ++ show (stateIndent st)
    traceM $ "Current state: " ++ show st
    -- Store original indent level
    origIndent <- gets stateIndent
    case stateTokens st of
        (tok:_) -> do
            traceM $ "Processing token: " ++ show tok
            traceM $ "Token column: " ++ show (locCol tok)
            case locToken tok of
                TWord "while" -> do
                    traceM "Found while expression at top-level"
                    expr <- parseWhileExpr Nothing
                    return $ TLExpr expr
                TWord "var" -> do
                    traceM "Found variable declaration at top-level"
                    expr <- parseVarDecl
                    return $ TLExpr expr
                TType -> do
                    traceM $ "\n=== Parsing type block ==="
                    traceM $ "Original indent: " ++ show origIndent
                    traceM $ "Creating type block context"

                    _ <- matchToken (== TType) "type"
                    let typeBlockIndent = locCol tok + 2

                    -- Create new context for type block
                    let typeCtx = IndentContext
                            { baseIndent = typeBlockIndent
                            , parentIndent = origIndent
                            , blockType = BasicBlock }  -- Change from TopLevel

                    traceM $ "New type context: " ++ show typeCtx
                    modify $ \s -> s { stateIndent = typeBlockIndent }

                    -- Parse remaining type declarations with new context
                    parseSingleTypeDecl
                TWord "print" -> do
                    traceM "Found print statement at top-level"
                    if origIndent > 0
                        then do
                            traceM $ "Checking print indentation against level: " ++ show origIndent
                            when (locCol tok < origIndent) $
                                throwError $ IndentationError $ IndentationErrorDetails origIndent (locCol tok) GreaterEq
                            expr <- parsePrintStatement
                            return $ TLExpr expr
                        else do
                            expr <- parsePrintStatement
                            return $ TLExpr expr
                TWord "block" -> do
                    traceM "Found block at top level - parsing directly"
                    expr <- parseBlockImpl  -- Parse block directly
                    return $ TLExpr expr    -- Don't wrap in extra block
                TWord "let" -> do
                    traceM "Found let block at top-level"
                    expr <- parseLetBlock
                    traceM $ "Parsed let block: " ++ show expr
                    return $ TLExpr expr
                TWord "fn" -> do
                    traceM "Found function declaration at top-level"
                    -- Save current indent before parsing function
                    modify $ \s -> s { stateIndent = 0 }
                    -- Parse the full function declaration
                    decl <- parseFuncDecl
                    -- Restore indent after function
                    modify $ \s -> s { stateIndent = origIndent }
                    traceM $ "Parsed function declaration: " ++ show decl
                    return $ TLDecl decl
                TWord name | isValidName (locToken tok) -> do
                    traceM $ "Found identifier: " ++ name
                    traceM $ "Token indent: " ++ show (locCol tok)
                    traceM $ "State indent: " ++ show (stateIndent st)

                    case blockType ctx of  -- Check passed context
                        BasicBlock | locCol tok == stateIndent st -> do
                            traceM $ "Parsing as type declaration"
                            parseSingleTypeDecl
                        _ -> do
                            traceM $ "Parsing as expression"
                            expr <- parseExpression
                            return $ TLExpr expr
                _ -> do
                    traceM $ "Unexpected token in top level: " ++ show tok
                    throwError $ UnexpectedToken tok "expected 'print', 'let', or 'block'"
        [] -> do
            traceM "No tokens available for top-level expression"
            throwError $ EndOfInput "expected top-level expression"

parseLetBlock :: Parser Expr
parseLetBlock = do
    traceM "Parsing let binding"
    _ <- matchToken (\t -> t == TWord "let") "let keyword"

    -- Parse first binding
    (varName, value) <- parseSingleBindingLine
    traceM $ "First binding parsed: Let " ++ show varName ++ " " ++ show value

    -- Check for more bindings
    moreBindings <- parseMoreBindings 2
    traceM $ "Additional bindings parsed: " ++ show moreBindings

    case moreBindings of
        [] -> do
            traceM "Single let binding - returning direct Let expression"
            return $ Let varName value
        _ -> do
            traceM "Multiple bindings - wrapping in block"
            let allBindings = Let varName value : map (\(n,v) -> Let n v) moreBindings
            return $ Block "top_let" allBindings Nothing
  where
    parseMoreBindings :: Int -> Parser [(String, Expr)]
    parseMoreBindings indent = do
        st <- get
        traceM $ "Checking for more bindings at indent " ++ show indent ++ ": " ++ show (take 3 $ stateTokens st)
        case stateTokens st of
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

parseSingleTypeDecl :: Parser TopLevel
parseSingleTypeDecl = do
    st <- get
    traceM $ "\n=== parseSingleTypeDecl ==="
    traceM $ "Current indent: " ++ show (stateIndent st)
    traceM $ "Current tokens: " ++ show (take 3 $ stateTokens st)
    typeNameTok <- matchToken isValidName "type name"
    typeParams <- parseTypeParams
    _ <- matchToken (== (TOperator "=")) "equals sign"
    case locToken typeNameTok of
        TWord name -> do
            traceM $ "Parsing type definition for: " ++ name
            typeDefinition <- parseTypeDefinition (T.pack name) typeParams
            return $ TLType name typeDefinition
        _ -> throwError $ UnexpectedToken typeNameTok "type name"

parseTypeDefinition :: T.Text -> [String] -> Parser Type
parseTypeDefinition givenName params = do
    traceM $ "\n=== parseTypeDefinition ==="
    traceM $ "Parsing type: " ++ T.unpack givenName

    -- Match struct keyword directly - equals was already consumed
    _ <- matchToken (== TStruct) "struct"

    fields <- parseStructFields
    traceM $ "Parsed fields: " ++ show fields

    st <- gets stateSymTable
    traceM $ "Initial symbol table: " ++ show st

    let name = T.unpack givenName
    let (sid, newSt) = if null params
                       then registerStruct name fields st
                       else registerParamStruct name params fields st

    modify $ \s -> s { stateSymTable = newSt }
    return $ TypeStruct sid name

parseStructFields :: Parser [(String, Type)]
parseStructFields = do
    traceM $ "\n=== Parsing struct fields ==="
    -- Get current state for indentation check
    st <- get
    traceM $ "Current state: " ++ show st

    let fieldBaseIndent = case stateTokens st of
          (tok:_) -> locCol tok - 2  -- Base indent is 2 less than field indent
          _ -> 0
    traceM $ "Field base indentation level: " ++ show fieldBaseIndent

    let loop acc = do
          st <- get
          traceM $ "\n=== Struct fields loop ==="
          traceM $ "Field base indent: " ++ show fieldBaseIndent
          traceM $ "Current tokens: " ++ show (take 3 $ stateTokens st)

          case stateTokens st of
            (tok:_)
              | isValidName (locToken tok) && locCol tok > fieldBaseIndent -> do
                  traceM $ "Parsing field at col " ++ show (locCol tok)
                  field <- parseStructField
                  traceM $ "Parsed struct field: " ++ show field
                  loop (field : acc)
              | locCol tok <= fieldBaseIndent -> do
                  traceM $ "Found token at col " ++ show (locCol tok) ++
                          " <= base indent " ++ show fieldBaseIndent ++
                          ", ending struct fields"
                  return $ reverse acc
            _ -> do
                traceM "No more tokens"
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
        TWord "i32" -> do
            traceM "Recognized Int32"
            return $ TypeNum Int32
        TWord "i64" -> do
            traceM "Recognized Int64"
            return $ TypeNum Int64
        TWord "f32" -> do
            traceM "Recognized Float32"
            return $ TypeNum Float32
        TWord "f64" -> do
            traceM "Recognized Float64"
            return $ TypeNum Float64
        TWord typeName -> do
            st <- get
            case stateTokens st of
                -- Handle generic type reference (e.g. Box[T])
                (next:_) | locToken next == TLeftBracket -> do
                    traceM $ "Parsing generic type reference: " ++ typeName
                    -- Parse the type parameter
                    params <- parseTypeParams
                    -- Look up base struct in symbol table
                    case M.lookup typeName (structNames $ stateSymTable st) of
                        Just sid -> do
                            -- Return generic type reference using struct ID
                            return $ TypeStruct sid typeName
                        Nothing -> do
                            -- Still allow parsing even if struct not found yet
                            -- Type checking will validate later
                            return $ TypeStruct (StructId 0) typeName

                -- Original cases for basic types and type parameters
                _ -> case typeName of
                    "i32" -> return $ TypeNum Int32
                    "i64" -> return $ TypeNum Int64
                    "f32" -> return $ TypeNum Float32
                    "f64" -> return $ TypeNum Float64
                    -- Single uppercase letter is a type parameter
                    _ | length typeName == 1 && isUpper (head typeName) ->
                        return $ TypeParam typeName
                    _ -> throwError $ UnexpectedToken tok "type name"
        _ -> throwError $ UnexpectedToken tok "type name"
  where
    isUpper c = c >= 'A' && c <= 'Z'
