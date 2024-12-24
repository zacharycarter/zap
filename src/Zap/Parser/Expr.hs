{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Expr
  ( ExprParser(..)
  , defaultExprParser
  , mapLexError
  , parseExpr
  , parseExprFromText
  , parseExpression
  , parseLetBinding
  , parsePrintStatement
  , parseSingleBindingLine
  , parseFuncDecl
  , parseVarDecl
  , parseWhileExpr
  , isPrint
  , isStringLit
  , isValidName
  ) where

import Control.Monad (when, replicateM)
import Control.Monad.State
import Control.Monad.Error.Class
import qualified Data.Text as T
import Debug.Trace

import Zap.Analysis.Lexical
import Zap.Parser.Types
import Zap.Parser.Core
import Zap.AST

data ExprParser = ExprParser
  { parseBasicExpr :: Parser Expr
  , parseBlock :: Parser Expr
  , parseBreak :: Parser Expr
  , parseResult :: Parser Expr
  }

-- Token validation helpers
isValidName :: Token -> Bool
isValidName (TWord name) = traceShow ("Validating name: " ++ name) $
  not $ name `elem` ["break", "result", "block", "let"]
isValidName _ = False

isBlockKeyword :: Token -> Bool
isBlockKeyword (TWord "block") = True
isBlockKeyword _ = False

isColon :: Token -> Bool
isColon TColon = True
isColon _ = False

isBreak :: Token -> Bool
isBreak (TWord "break") = True
isBreak _ = False

isPrint :: Token -> Bool
isPrint (TWord "print") = True
isPrint _ = False

isResult :: Token -> Bool
isResult (TWord "result") = True
isResult _ = False

isStringLit :: Token -> Bool
isStringLit (TString _) = True
isStringLit _ = False

isNumber :: Token -> Bool
isNumber (TNumber _) = True
isNumber _ = False

isOperator :: Token -> Bool
isOperator (TOperator op) = op `elem` ["+","-","*","/","<", ">", "=", "&", "|", "+="]
isOperator _ = False

isVecConstructor :: Token -> Bool
isVecConstructor (TVec _) = True
isVecConstructor _ = False

-- Main expression parser
parseExpr :: ExprParser -> Parser Expr
parseExpr parsers = do
    state <- get
    traceM $ "parseExpr state: " ++ show (stateIndent state) ++ " tokens: " ++ show (take 3 $ stateTokens state)
    case stateTokens state of
      (tok:_) -> do
        traceM $ "Parsing expression starting with: " ++ show (locToken tok)
        case locToken tok of
          TWord "let" -> parseLetBinding parsers
          TWord "block" -> parseBlock parsers
          TWord "break" -> parseBreak parsers
          TWord "result" -> parseResult parsers
          TVec _ -> parseVectorLiteral
          TWord "print" -> parsePrintStatement
          _ -> parseExpression
      [] -> throwError $ EndOfInput "expression"

-- Top level expression parser that handles operator precedence
parseExpression :: Parser Expr
parseExpression = do
    traceM "Parsing complete expression"
    left <- parseComparison
    traceM $ "Parsed comparison expression: " ++ show left
    -- Add more detailed state logging
    state <- get
    traceM $ "Expression state before operator check: " ++ show (take 3 $ stateTokens state)
    case stateTokens state of
        (tok:_) -> case locToken tok of
            TOperator "+=" -> do
                traceM $ "Found += operator at col " ++ show (locCol tok)
                _ <- matchToken (\t -> t == TOperator "+=") "+="
                right <- parseComparison
                case left of
                    Var name -> do
                        traceM $ "Creating += assignment for " ++ name
                        return $ AssignOp name Add right
                    _ -> do
                        traceM $ "Invalid left side for +=: " ++ show left
                        throwError $ UnexpectedToken tok "variable name before +="
            _ -> do
                traceM $ "No += found, token was: " ++ show tok
                return left
        [] -> return left

parseComparison :: Parser Expr
parseComparison = do
    traceM "Parsing comparison expression"
    left <- parseAdditive
    remainingComparison left
  where
    remainingComparison left = do
      state <- get
      case stateTokens state of
        (tok:_) -> case locToken tok of
          TOperator "<" -> do
            traceM "Found less than operator"
            _ <- matchToken isOperator "<"
            right <- parseAdditive
            return $ BinOp Lt left right
          _ -> return left
        [] -> return left

-- Parse addition and subtraction with proper precedence
parseAdditive :: Parser Expr
parseAdditive = do
    traceM "Parsing additive expression"
    left <- parseMultiplicative
    remainingAdditive left
  where
    remainingAdditive left = do
      state <- get
      traceM $ "Parsing remaining additive with left: " ++ show left
      case stateTokens state of
        (tok:_) -> case locToken tok of
          TOperator "+" -> do
            traceM "Found addition operator"
            _ <- matchToken isOperator "+"
            right <- parseMultiplicative
            remainingAdditive (BinOp Add left right)
          TOperator "+=" -> do
            traceM "Found += operator"
            _ <- matchToken (\t -> t == TOperator "+=") "+="
            right <- parseMultiplicative
            case left of
              Var name -> return $ AssignOp name Add right
              _ -> throwError $ UnexpectedToken tok "variable for assignment"
          TOperator "-" -> do
            traceM "Found subtraction operator"
            _ <- matchToken isOperator "-"
            right <- parseMultiplicative
            remainingAdditive (BinOp Sub left right)
          _ -> return left
        [] -> return left

-- Parse multiplication and division
parseMultiplicative :: Parser Expr
parseMultiplicative = do
    traceM "Parsing multiplicative expression"
    left <- parseUnary
    remainingMultiplicative left
  where
    remainingMultiplicative left = do
      state <- get
      case stateTokens state of
        (tok:_) -> case locToken tok of
          TOperator "*" -> do
            _ <- matchToken isOperator "*"
            right <- parseUnary
            remainingMultiplicative (BinOp Mul left right)
          TOperator "/" -> do
            _ <- matchToken isOperator "/"
            right <- parseUnary
            remainingMultiplicative (BinOp Div left right)
          _ -> return left
        [] -> return left

-- Parse unary operations and basic terms
parseUnary :: Parser Expr
parseUnary = parseTerm

-- Parse basic terms (numbers, variables, vector literals)
parseTerm :: Parser Expr
parseTerm = do
    traceM "Parsing term"
    state <- get
    case stateTokens state of
        (tok:rest) -> case locToken tok of
            TWord name | isValidName (locToken tok) -> do
                _ <- matchToken isValidName "identifier"
                parseFieldOrCallChain (Var name)
            TWord "print" -> parsePrintStatement
            _ -> parseBaseTerm >>= parseFieldAccess
        [] -> throwError $ EndOfInput "term"

parseFieldOrCallChain :: Expr -> Parser Expr
parseFieldOrCallChain expr = do
    state <- get
    case stateTokens state of
        (tok:_) -> case locToken tok of
            TDot -> do
                _ <- matchToken (== TDot) "dot"
                fieldTok <- matchToken isValidName "field name"
                case locToken fieldTok of
                    TWord field -> parseFieldOrCallChain (FieldAccess expr field)
                    _ -> throwError $ UnexpectedToken fieldTok "field name"
            TLeftParen -> do
                _ <- matchToken (== TLeftParen) "("
                args <- parseCallArgs
                _ <- matchToken (== TRightParen) ")"
                case expr of
                    Var name -> parseFieldOrCallChain (Call name args)
                    _ -> throwError $ UnexpectedToken tok "expected a function name for the call"
            _ -> return expr
        [] -> return expr

parseBaseTerm :: Parser Expr
parseBaseTerm = do
    traceM "Parsing base term"
    state <- get
    case stateTokens state of
        (tok:rest) -> case locToken tok of
            TString s -> do
                traceM $ "Found string literal: " ++ s
                _ <- matchToken isStringLit "string literal"
                return $ StrLit s

            TNumber n -> do
                traceM $ "Found numeric literal: " ++ n
                _ <- matchToken isNumber "number"
                if '.' `elem` n
                    then return $ NumLit Float32 n
                    else return $ NumLit Int32 n

            TWord name | isValidName (locToken tok) -> do
                traceM $ "Found identifier: " ++ name
                _ <- matchToken isValidName "identifier"
                parseMaybeCall name

            TVec vecTypeStr -> do
                traceM $ "Found vector constructor"
                _ <- matchToken isVecConstructor "vector constructor"
                parseMaybeCall vecTypeStr

            _ -> throwError $ UnexpectedToken tok "term"
        [] -> throwError $ EndOfInput "term"

parseMaybeCall :: String -> Parser Expr
parseMaybeCall fname = do
    traceM $ "parseMaybeCall: fname = " ++ fname
    state <- get
    case stateTokens state of
        (tok:_)
            | locToken tok == TLeftParen -> do
                traceM $ "Found constructor/function call with parens"
                _ <- matchToken (== TLeftParen) "opening parenthesis"
                args <- parseCallArgs
                _ <- matchToken (== TRightParen) "closing parenthesis"
                return (Call fname args)
            | locToken tok == TComma || isCallArgToken (locToken tok) -> do
                traceM $ "Found constructor/function call with args"
                args <- parseCallArgs
                return (Call fname args)
            | otherwise -> do
                traceM $ "Found identifier: falling back to Var"
                return (Var fname)
        [] -> return (Var fname)
  where
    isCallArgToken :: Token -> Bool
    isCallArgToken (TNumber _) = True
    isCallArgToken (TString _) = True
    isCallArgToken (TWord name) = isValidName (TWord name)
    isCallArgToken (TVec _) = True
    isCallArgToken _ = False

parseCallArgs :: Parser [Expr]
parseCallArgs = do
    state <- get
    case stateTokens state of
        (tok:_)
            | locToken tok == TRightParen -> return []
            | otherwise -> do
                firstArg <- parseExpression
                parseMoreArgs [firstArg]
        [] -> return []
  where
    parseMoreArgs acc = do
        state <- get
        case stateTokens state of
            (tok:_) | locToken tok == TComma -> do
                _ <- matchToken (== TComma) "comma"
                arg <- parseExpression
                parseMoreArgs (acc ++ [arg])
            _ -> return acc

parseFieldAccess :: Expr -> Parser Expr
parseFieldAccess expr = do
    traceM "Checking for field access"
    state <- get
    case stateTokens state of
        (tok:_) | locToken tok == TDot -> do
            traceM "Found field access"
            _ <- matchToken (== TDot) "dot"
            fieldName <- matchToken isValidName "field name"
            case locToken fieldName of
                TWord name -> do
                    traceM $ "Accessing field: " ++ name
                    let nextExpr = FieldAccess expr name
                    parseFieldAccess nextExpr  -- Handle chained field access
                _ -> throwError $ UnexpectedToken fieldName "field name"
        _ -> return expr

parseBasicExprImpl :: Parser Expr
parseBasicExprImpl = do
    traceM "Starting basic expression parse"
    state <- get
    checkIndent GreaterEq
    case stateTokens state of
        [] -> throwError $ EndOfInput "expression expected"
        (tok:_) -> case locToken tok of
            TString s -> do
                _ <- matchToken isStringLit "string literal"
                return $ StrLit s
            TNumber n -> do
                _ <- matchToken isNumber "number"
                return $ NumLit Float32 n
            TVec vecTypeStr -> do
                traceM $ "Parsing vector constructor: " ++ vecTypeStr
                _ <- matchToken isVecConstructor "vector constructor"
                -- Parse as a call
                parseMaybeCall vecTypeStr
            TWord name | isValidName (locToken tok) -> do
                _ <- matchToken isValidName "identifier"
                parseMaybeCall name
            _ -> throwError $ UnexpectedToken tok "term"

parseLetBinding :: ExprParser -> Parser Expr
parseLetBinding _ = do
    traceM "Parsing let binding"
    _ <- matchToken (\t -> t == TWord "let") "let keyword"
    (varName, value) <- parseSingleBindingLine
    traceM $ "Completed let binding for: " ++ varName
    return $ Let varName value

-- Helper function to parse a single binding line (no 'let' keyword)
parseSingleBindingLine :: Parser (String, Expr)
parseSingleBindingLine = do
    traceM "Parsing single binding line (identifier = expression)"
    checkIndent GreaterEq
    nameTok <- matchToken isValidName "identifier"
    _ <- matchToken (== TEquals) "equals sign"
    traceM "Parsing value for binding line"
    value <- parseExpression
    case locToken nameTok of
        TWord varName -> do
            traceM $ "Single binding line completed for: " ++ varName
            return (varName, value)
        _ -> throwError $ UnexpectedToken nameTok "identifier"

parseBlockImpl :: Parser Expr
parseBlockImpl = do
    state <- get
    traceM $ "Starting block parse with indent: " ++ show (stateIndent state)
    case stateTokens state of
      (tok:_) -> do
        traceM $ "Block token: " ++ show tok
        blockTok <- matchToken isBlockKeyword "block"
        nameTok <- matchToken isValidName "block name"
        traceM $ "Block name: " ++ show (locToken nameTok)
        _ <- matchToken isColon ":"
        let currentIndent = locCol blockTok
        let newIndent = currentIndent + 2
        traceM $ "Setting block indent from " ++ show currentIndent ++ " to " ++ show newIndent
        modify (\s -> s { stateIndent = newIndent })
        (exprs, resultExpr) <- parseBlockExprs newIndent
        traceM $ "Block contents parsed: " ++ show (length exprs) ++ " expressions"
        return $ Block $ BlockScope
          { blockLabel = case locToken nameTok of
                          TWord name -> name
                          _ -> error "Invalid block name"
          , blockExprs = exprs
          , blockResult = resultExpr
          }
      [] -> throwError $ EndOfInput "block"

parseBlockExprs :: Int -> Parser ([Expr], Maybe Expr)
parseBlockExprs baseIndent = do
    state <- get
    traceM $ "parseBlockExprs at indent " ++ show baseIndent ++ " with tokens: " ++ show (take 3 $ stateTokens state)
    case stateTokens state of
        [] -> return ([], Nothing)
        (tok:_) ->
            if locToken tok == TEOF
                then return ([], Nothing)
                else do
                    when (locCol tok < baseIndent) $
                        throwError $ IndentationError baseIndent (locCol tok) GreaterEq
                    case locToken tok of
                        TWord "result" -> do
                            traceM "Parsing result expression"
                            resultExpr <- parseResultImpl
                            breaks <- parsePostResult baseIndent
                            traceM $ "Result parsed with " ++ show (length breaks) ++ " following breaks"
                            return (breaks, Just resultExpr)
                        TWord "break" -> do
                            traceM "Parsing break statement"
                            breakExpr <- parseBreakImpl
                            (moreExprs, resultExpr) <- parseBlockExprs baseIndent
                            return (breakExpr : moreExprs, resultExpr)
                        TWord "block" -> do
                            traceM "Parsing nested block"
                            blockExpr <- parseBlockImpl
                            (moreExprs, resultExpr) <- parseBlockExprs baseIndent
                            return (blockExpr : moreExprs, resultExpr)
                        TWord "print" -> do
                            traceM "Found print statement in block"
                            printExpr <- parsePrintStatement
                            (moreExprs, resultExpr) <- parseBlockExprs baseIndent
                            return (printExpr : moreExprs, resultExpr)
                        _ -> do
                            traceM "Parsing block expression"
                            expr <- parseExpression  -- Change from parseBasicExpr to parseExpression
                            (moreExprs, resultExpr) <- parseBlockExprs baseIndent
                            return (expr : moreExprs, resultExpr)

parseBreakImpl :: Parser Expr
parseBreakImpl = do
    state <- get
    traceM $ "parseBreak at indent " ++ show (stateIndent state)
    checkIndent GreaterEq
    breakTok <- matchToken isBreak "break"
    labelTok <- matchToken isValidName "block label"
    traceM $ "Break to label: " ++ show (locToken labelTok)
    case locToken labelTok of
      TWord label -> return $ Break label
      _ -> error "Matched non-word token as break label"

parseResultImpl :: Parser Expr
parseResultImpl = do
    state <- get
    traceM $ "parseResult at indent " ++ show (stateIndent state)
    checkIndent GreaterEq
    resultTok <- matchToken isResult "result"
    traceM "Parsing result expression value"
    expr <- parseBasicExprImpl
    return $ Result expr

parsePostResult :: Int -> Parser [Expr]
parsePostResult baseIndent = do
    state <- get
    traceM $ "parsePostResult at indent " ++ show baseIndent
    case stateTokens state of
      [] -> return []
      (tok:_) -> do
        traceM $ "Post-result token: " ++ show tok
        if locToken tok == TEOF
            then return []
            else do
                when (locCol tok < baseIndent) $
                    throwError $ IndentationError baseIndent (locCol tok) GreaterEq
                case locToken tok of
                    TWord "break" -> do
                        traceM "Parsing post-result break statement"
                        breakExpr <- parseBreakImpl
                        rest <- parsePostResult baseIndent
                        return (breakExpr : rest)
                    TWord "result" ->
                        throwError $ UnexpectedToken tok "only break statements allowed after result"
                    _ -> throwError $ UnexpectedToken tok "only break statements allowed after result"

parseVectorLiteral :: Parser Expr
parseVectorLiteral = do
    constructorTok <- matchToken isVecConstructor "vector constructor"
    case locToken constructorTok of
        TVec vecTypeStr -> do
            traceM $ "Parsing vector constructor: " ++ show vecTypeStr
            -- After reading the constructor, parse as a call
            parseMaybeCall vecTypeStr
        _ -> throwError $ UnexpectedToken constructorTok "vector constructor"

parsePrintStatement :: Parser Expr
parsePrintStatement = do
    traceM "Parsing print statement"
    expectedIndent <- gets stateIndent
    state <- get
    let tok = head $ stateTokens state
    when (locCol tok < expectedIndent) $
        throwError $ IndentationError expectedIndent (locCol tok) GreaterEq
    _ <- matchToken isPrint "print"

    -- Look ahead to see if there's a left paren
    state' <- get
    case stateTokens state' of
        (tok:_) | locToken tok == TLeftParen -> do
            -- Function-style print with parentheses
            _ <- matchToken (== TLeftParen) "("
            expr <- parseExpression
            _ <- matchToken (== TRightParen) ")"
            return $ Call "print" [expr]
        _ -> do
            -- Traditional style print without parentheses
            expr <- parseExpression  -- This will handle variable references
            return $ Call "print" [expr]

parseWhileExpr :: Parser Expr
parseWhileExpr = do
    traceM "Parsing while expression"
    _ <- matchToken (\t -> t == TWord "while") "while keyword"
    condition <- parseExpression
    _ <- matchToken isColon ":"
    bodyState <- get
    traceM $ "While loop body state: " ++ show bodyState
    let currentIndent = stateIndent bodyState
    let newIndent = currentIndent + 2
    modify (\s -> s { stateIndent = newIndent })
    (bodyExprs, mResult) <- parseBlockExprs newIndent
    let blockScope = BlockScope "while_body" bodyExprs mResult
    let body = Block blockScope
    traceM $ "Parsed while body: " ++ show body
    return $ While condition body

parseWhileBody :: Int -> Parser Expr
parseWhileBody indent = do
    (bodyExprs, bodyResult) <- parseBlockExprs indent
    case bodyExprs of
        [] -> throwError $ EndOfInput "while body"
        _ -> return $ Block $ BlockScope
            { blockLabel = "while_body"
            , blockExprs = bodyExprs
            , blockResult = bodyResult
            }

parseFuncDecl :: Parser Decl
parseFuncDecl = do
    traceM "Parsing function declaration"
    _ <- matchToken (\t -> t == TWord "fn") "fn"
    nameTok <- matchToken isValidName "function name"
    _ <- matchToken (== TLeftParen) "("

    -- Parse parameters
    params <- parseParams
    traceM $ "Parsed parameters: " ++ show params

    -- Parse return type
    _ <- matchToken (== TColon) ":"
    retTypeTok <- matchToken isValidName "return type"
    retType <- parseTypeToken retTypeTok

    _ <- matchToken (== TEquals) "="
    body <- parseExpression

    case locToken nameTok of
        TWord name -> return $ DFunc name params retType body
        _ -> throwError $ UnexpectedToken nameTok "function name"

-- Helper to parse parameters with shared type annotation
parseParams :: Parser [Param]
parseParams = do
    traceM "Parsing function parameters"
    -- Parse comma-separated parameter names
    names <- parseParamNames
    traceM $ "Parsed parameter names: " ++ show names

    -- Parse shared type annotation
    _ <- matchToken (== TColon) ":"
    typeTok <- matchToken isValidName "parameter type"
    paramType <- parseTypeToken typeTok
    traceM $ "Parsed parameter type: " ++ show paramType

    _ <- matchToken (== TRightParen) ")"

    -- Create params with shared type
    return [Param name paramType | name <- names]
  where
    parseParamNames :: Parser [String]
    parseParamNames = do
        nameTok <- matchToken isValidName "parameter name"
        case locToken nameTok of
            TWord name -> do
                state <- get
                case stateTokens state of
                    (tok:_) | locToken tok == TComma -> do
                        _ <- matchToken (== TComma) ","
                        rest <- parseParamNames
                        return (name : rest)
                    _ -> return [name]
            _ -> throwError $ UnexpectedToken nameTok "parameter name"

-- Helper to parse type tokens
parseTypeToken :: Located -> Parser Type
parseTypeToken tok = case locToken tok of
    TWord "i32" -> return $ TypeNum Int32
    TWord "f32" -> return $ TypeNum Float32
    TWord other -> throwError $ UnexpectedToken tok "valid type"

parseVarDecl :: Parser Expr
parseVarDecl = do
    traceM "Parsing variable declaration"
    _ <- matchToken (\t -> t == TWord "var") "var"
    nameTok <- matchToken isValidName "identifier"
    _ <- matchToken (== TEquals) "equals sign"
    value <- parseExpression
    case locToken nameTok of
        TWord name -> return $ VarDecl name value
        _ -> throwError $ UnexpectedToken nameTok "identifier"

parseExprFromText :: T.Text -> Either ParseError Expr
parseExprFromText input = do
    tokens <- mapLexError $ tokenize input
    runParser parseExpression tokens

vecTypeFromString :: String -> VecType
vecTypeFromString "Vec2" = Vec2 Float32
vecTypeFromString "Vec3" = Vec3 Float32
vecTypeFromString "Vec4" = Vec4 Float32
vecTypeFromString other = error $ "Invalid vector type: " ++ other

defaultExprParser :: ExprParser
defaultExprParser = ExprParser
  { parseBasicExpr = parseBasicExprImpl
  , parseBlock = parseBlockImpl
  , parseBreak = parseBreakImpl
  , parseResult = parseResultImpl
  }

mapLexError :: Either LexError a -> Either ParseError a
mapLexError (Left (UnterminatedString line col)) =
    Left $ EndOfInput $ "Unterminated string at line " ++ show line ++ ", column " ++ show col
mapLexError (Left (InvalidCharacter c line col)) =
    Left $ EndOfInput $ "Invalid character '" ++ [c] ++ "' at line " ++ show line ++ ", column " ++ show col
mapLexError (Right a) = Right a
