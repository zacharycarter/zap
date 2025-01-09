{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Expr
  ( ExprParser(..)
  , defaultExprParser
  , mapLexError
  , parseAssign
  , parseBlockExprs
  , parseExpr
  , parseExprFromText
  , parseExpression
  , parseLetBinding
  , parsePrintStatement
  , parseSingleBindingLine
  , parseBlockImpl
  , parseFuncDecl
  , parseVarDecl
  , parseWhileExpr
  , parseMaybeCall
  , isPrint
  , isStringLit
  , isValidName
  ) where

import Control.Monad (when)
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
isValidName (TWord name) = do
  let nameHd = case name of
        [] -> '_'
        hd : _ -> hd
  traceShow ("Validating name: " ++ name) $
    not (name `elem` ["break", "result", "block", "let", "var", "fn", "print", "if", "while"]) &&
    not (null name) &&
    (isAlpha nameHd)
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
    st <- get
    traceM $ "parseExpr state: " ++ show (stateIndent st) ++ " tokens: " ++ show (take 3 $ stateTokens st)
    case stateTokens st of
      (tok:_) -> do
        traceM $ "Parsing expression starting with: " ++ show (locToken tok)
        case locToken tok of
          TWord "let" -> parseLetBinding parsers
          TWord "block" -> parseBlock parsers
          TWord "break" -> parseBreak parsers
          TWord "result" -> parseResult parsers
          TVec _ -> parseVectorLiteral
          TWord "print" -> parsePrintStatement
          TWord "var" -> parseVarDecl
          _ -> parseExpression
      [] -> throwError $ EndOfInput "expression"

-- Top level expression parser that handles operator precedence
parseExpression :: Parser Expr
parseExpression = parseExpressionWithType Nothing

parseExpressionWithType :: Maybe Type -> Parser Expr
parseExpressionWithType expectedType = do
  traceM "Parsing complete expression"
  st <- get
  case stateTokens st of
        (tok:_)
          | locToken tok == TWord "if" -> parseIf expectedType
          | locToken tok == TWord "while" -> parseWhileExpr expectedType
        _ -> parseAssign expectedType >>= remainingExpression expectedType

parseIf :: Maybe Type -> Parser Expr
parseIf expectedType = do
    traceM "Parsing if statement"
    _ <- matchToken isIf "if"
    condition <- parseExpressionWithType expectedType
    _ <- matchToken isColon ":"

    -- Parse then branch with proper indentation
    st <- get
    let curIndent = stateIndent st
    let newIndent = curIndent + 2
    modify $ \s -> s { stateIndent = newIndent }

    thenStmts <- parseBlockExprs expectedType BasicBlock newIndent

    -- Convert statements to block
    let thenBlock = Block $ BlockScope
          { blockLabel = "if_then"
          , blockExprs = fst thenStmts
          , blockResult = snd thenStmts
          }

    -- Reset indentation
    modify $ \s -> s { stateIndent = curIndent }

    -- Since this is a simple if without else, pass Nothing for else branch
    return $ If condition thenBlock (Lit (BooleanLit False))

-- Helper for if keyword
isIf :: Token -> Bool
isIf (TWord "if") = True
isIf _ = False

remainingExpression :: Maybe Type -> Expr -> Parser Expr
remainingExpression expectedType left = do
    st <- get
    traceM $ "Processing remaining expression with left: " ++ show left
    case stateTokens st of
        (tok:_) -> do
            traceM $ "Next token in remaining expression: " ++ show (locToken tok)
            case locToken tok of
                TOperator "*" -> do
                    traceM "Found multiplication operator"
                    _ <- matchToken isOperator "*"
                    right <- parseAssign expectedType
                    remainingExpression expectedType (BinOp Mul left right)
                TOperator "/" -> do
                    traceM "Found division operator"
                    _ <- matchToken isOperator "/"
                    right <- parseAssign expectedType
                    remainingExpression expectedType (BinOp Div left right)
                _ -> return left
        [] -> return left

parseAssign :: Maybe Type -> Parser Expr
parseAssign expectedType = do
    traceM "Parsing assignment"
    left <- parseComparison expectedType
    traceM $ "Parsed comparison: " ++ show left
    st <- get
    traceM $ "Expression state before operator check: " ++ show (take 3 $ stateTokens st)
    case stateTokens st of
        (tok:_) -> case locToken tok of
            TOperator "+=" -> do
                traceM $ "Found += operator at col " ++ show (locCol tok)
                _ <- matchToken (\t -> t == TOperator "+=") "+="
                right <- parseComparison expectedType
                case left of
                    Var name -> do
                        traceM $ "Creating += assignment for " ++ name
                        return $ AssignOp name Add right
                    _ -> do
                        traceM $ "Invalid left side for +=: " ++ show left
                        throwError $ UnexpectedToken tok "variable name before +="
            TEquals -> do
                traceM $ "Found = operator at col " ++ show (locCol tok)
                _ <- matchToken (\t -> t == TEquals) "="
                right <- parseAssign expectedType
                case left of
                    Var name -> do
                        traceM $ "Creating = assignment for " ++ name
                        return $ Assign name right
                    _ -> do
                        traceM $ "Invalid left side for =: " ++ show left
                        throwError $ UnexpectedToken tok "variable name before ="
            _ -> do
                traceM $ "No assignment operator found, token was: " ++ show tok
                return left
        [] -> return left

parseComparison :: Maybe Type -> Parser Expr
parseComparison expectedType = do
    traceM "Parsing comparison expression"
    left <- parseAdditive expectedType
    traceM $ "Parsed additive: " ++ show left
    remainingComparison expectedType left
  where
    remainingComparison expectedType left = do
      st <- get
      case stateTokens st of
        (tok:_) -> case locToken tok of
          TOperator "<" -> do
            traceM "Found less than operator"
            _ <- matchToken isOperator "<"
            right <- parseAdditive expectedType
            remainingComparison expectedType (BinOp Lt left right)
          TOperator ">" -> do
            traceM "Found greater than operator"
            _ <- matchToken isOperator ">"
            right <- parseAdditive expectedType
            remainingComparison expectedType (BinOp Gt left right)
          TEqualsEquals -> do
            traceM "Found equality operator"
            _ <- matchToken (== TEqualsEquals) "=="
            right <- parseAdditive expectedType
            remainingComparison expectedType (BinOp EqEq left right)
          _ -> return left
        [] -> return left

-- Parse addition and subtraction with proper precedence
parseAdditive :: Maybe Type -> Parser Expr
parseAdditive expectedType = do
    traceM "Parsing additive expression"
    left <- parseMultiplicative expectedType
    remainingAdditive expectedType left
  where
    remainingAdditive expectedType left = do
      st <- get
      traceM $ "Parsing remaining additive with left: " ++ show left
      case stateTokens st of
        (tok:_) -> case locToken tok of
          TOperator "+" -> do
            traceM "Found addition operator"
            _ <- matchToken isOperator "+"
            right <- parseMultiplicative expectedType
            remainingAdditive expectedType (BinOp Add left right)
          TOperator "+=" -> do
            traceM "Found += operator"
            _ <- matchToken (\t -> t == TOperator "+=") "+="
            right <- parseMultiplicative expectedType
            case left of
              Var name -> return $ AssignOp name Add right
              _ -> throwError $ UnexpectedToken tok "variable for assignment"
          TOperator "-" -> do
            traceM "Found subtraction operator"
            _ <- matchToken isOperator "-"
            right <- parseMultiplicative expectedType
            remainingAdditive expectedType (BinOp Sub left right)
          _ -> return left
        [] -> return left

-- Parse multiplication and division
parseMultiplicative :: Maybe Type -> Parser Expr
parseMultiplicative expectedType = do
    traceM "Parsing multiplicative expression"
    left <- parseUnary expectedType
    remainingMultiplicative expectedType left
  where
    remainingMultiplicative expectedType left = do
      st <- get
      case stateTokens st of
        (tok:_) -> case locToken tok of
          TOperator "*" -> do
            _ <- matchToken isOperator "*"
            right <- parseUnary expectedType
            remainingMultiplicative expectedType (BinOp Mul left right)
          TOperator "/" -> do
            _ <- matchToken isOperator "/"
            right <- parseUnary expectedType
            remainingMultiplicative expectedType (BinOp Div left right)
          _ -> return left
        [] -> return left

-- Parse unary operations and basic terms
parseUnary :: Maybe Type -> Parser Expr
parseUnary expectedType = parseTerm expectedType

-- Parse basic terms (numbers, variables, vector literals)
parseTerm :: Maybe Type -> Parser Expr
parseTerm expectedType = do
    traceM "Parsing term"
    st <- get
    case stateTokens st of
        (tok:_) -> do
            traceM $ "Parsing term starting with token: " ++ show tok
            case locToken tok of
                TNumber n -> do
                    traceM $ "Found number token: " ++ n
                    _ <- matchToken isNumber "number"
                    -- Look ahead for type suffix
                    st' <- get
                    case stateTokens st' of
                        (suffixTok:_) | isTypeSuffix (locToken suffixTok) -> do
                            traceM $ "Found type suffix after number"
                            _ <- matchToken isTypeSuffix "type suffix"
                            let typ = case locToken suffixTok of
                                    TTypeSuffix s -> parseNumType s
                                    _ -> error "Impossible: non-suffix token passed isTypeSuffix"
                            traceM $ "Parsed number with suffix type: " ++ show typ
                            buildNumericLiteral typ n

                        _ -> do
                            traceM "No type suffix found, checking expected type"
                            -- Use expected type if available, otherwise infer
                            let inferredType = if '.' `elem` n
                                             then Float64
                                             else Int64
                            let typ = case expectedType of
                                  Just (TypeNum t) -> t
                                  _ -> inferredType
                            traceM $ "Using type: " ++ show typ
                            buildNumericLiteral typ n
                    where
                      buildNumericLiteral :: NumType -> String -> Parser Expr
                      buildNumericLiteral typ val = do
                          traceM $ "Building numeric literal with type " ++ show typ
                          let expr = case typ of
                                  Float32 -> Lit (FloatLit val (Just Float32))
                                  Float64 -> Lit (FloatLit val (Just Float64))
                                  Int32 -> Lit (IntLit val (Just Int32))
                                  Int64 -> Lit (IntLit val (Just Int64))
                          parseFieldOrCallChain expr
                TLeftParen -> do
                    traceM "Found opening parenthesis"
                    _ <- matchToken (== TLeftParen) "("
                    expr <- parseExpression
                    traceM $ "Parsed expression in parentheses: " ++ show expr
                    _ <- matchToken (== TRightParen) ")"
                    -- Continue parsing any following operators
                    parseFieldOrCallChain expr
                TWord name | isValidName (locToken tok) -> do
                    _ <- matchToken isValidName "identifier"
                    parseFieldOrCallChain (Var name)
                TWord "print" -> parsePrintStatement
                TWord "var" -> parseVarDecl
                _ -> parseBaseTerm >>= parseFieldAccess
        [] -> throwError $ EndOfInput "term"

createNumericLiteral :: NumType -> String -> Expr
createNumericLiteral typ val = case typ of
    Float32 -> Lit (FloatLit val (Just Float32))
    Float64 -> Lit (FloatLit val (Just Float64))
    Int32 -> Lit (IntLit val (Just Int32))
    Int64 -> Lit (IntLit val (Just Int64))

parseFieldOrCallChain :: Expr -> Parser Expr
parseFieldOrCallChain expr = do
    st <- get
    case stateTokens st of
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
    st <- get
    case stateTokens st of
        (tok:_) -> case locToken tok of
            TLeftParen -> do
                traceM "Found opening parenthesis in base term"
                _ <- matchToken (== TLeftParen) "("
                expr <- parseExpression
                _ <- matchToken (== TRightParen) ")"
                traceM $ "Parsed parenthesized expression: " ++ show expr
                return expr

            TString s -> do
                traceM $ "Found string literal: " ++ s
                _ <- matchToken isStringLit "string literal"
                return $ Lit $ StringLit s

            TNumber n -> do
                traceM $ "Found numeric literal: " ++ n
                _ <- matchToken isNumber "number"
                if '.' `elem` n
                    then return $ Lit (FloatLit n (Just Float32))
                    else return $ Lit (IntLit n (Just Int32))

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
    st <- get
    case stateTokens st of
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
    st <- get
    case stateTokens st of
        (tok:_)
            | locToken tok == TRightParen -> return []
            | otherwise -> do
                firstArg <- parseExpression
                parseMoreArgs [firstArg]
        [] -> return []
  where
    parseMoreArgs acc = do
        st <- get
        case stateTokens st of
            (tok:_) | locToken tok == TComma -> do
                _ <- matchToken (== TComma) "comma"
                arg <- parseExpression
                parseMoreArgs (acc ++ [arg])
            _ -> return acc

parseFieldAccess :: Expr -> Parser Expr
parseFieldAccess expr = do
    traceM "Checking for field access"
    st <- get
    case stateTokens st of
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
    st <- get
    checkIndent GreaterEq
    case stateTokens st of
        [] -> throwError $ EndOfInput "expression expected"
        (tok:_) -> case locToken tok of
            TString s -> do
                _ <- matchToken isStringLit "string literal"
                return $ Lit $ StringLit s
            TNumber n -> do
                traceM $ "Found number token: " ++ n
                _ <- matchToken isNumber "number"
                -- Look ahead for type suffix
                st' <- get
                case stateTokens st' of
                    (suffixTok:_) | isTypeSuffix (locToken suffixTok) -> do
                        traceM $ "Found type suffix: " ++ show suffixTok
                        _ <- matchToken isTypeSuffix "type suffix"
                        let typ = case locToken suffixTok of
                                TTypeSuffix s -> parseNumType s
                                _ -> error "Impossible: non-suffix token passed isTypeSuffix"
                        case typ of
                          Int32 -> return $ Lit (IntLit n (Just Int32))
                          Int64 -> return $ Lit (IntLit n (Just Int64))
                          Float32 -> return $ Lit (FloatLit n (Just Float32))
                          Float64 -> return $ Lit (FloatLit n (Just Float64))
                    _ -> do
                        traceM "No type suffix found, defaulting to Int64/Float64"
                        -- Default to 64-bit types
                        let typ = if '.' `elem` n then Float64 else Int64
                        case typ of
                          Int64 -> return $ Lit (IntLit n (Just Int64))
                          Float64 -> return $ Lit (FloatLit n (Just Float64))
            TVec vecTypeStr -> do
                traceM $ "Parsing vector constructor: " ++ vecTypeStr
                _ <- matchToken isVecConstructor "vector constructor"
                -- Parse as a call
                parseMaybeCall vecTypeStr
            TWord name | isValidName (locToken tok) -> do
                _ <- matchToken isValidName "identifier"
                parseMaybeCall name
            _ -> throwError $ UnexpectedToken tok "term"

-- Helper to check for type suffix tokens
isTypeSuffix :: Token -> Bool
isTypeSuffix (TTypeSuffix _) = True
isTypeSuffix _ = False

-- Helper to convert type suffix to NumType
parseNumType :: String -> NumType
parseNumType "i32" = Int32
parseNumType "i64" = Int64
parseNumType "f32" = Float32
parseNumType "f64" = Float64
parseNumType t = error $ "Invalid numeric type suffix: " ++ t

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

    -- Optional type annotation
    annotatedType <- do
        st <- get
        case stateTokens st of
            (tok:_) | locToken tok == TColon -> do
                traceM "Found type annotation"
                _ <- matchToken (== TColon) ":"
                typeTok <- matchToken isValidName "type name"
                declaredType <- parseTypeToken typeTok
                return $ Just declaredType
            _ -> return Nothing

    _ <- matchToken (== TEquals) "equals sign"
    traceM "Parsing value for binding line"
    value <- parseExpressionWithType annotatedType

    -- If we have a type annotation, ensure value matches it
    case annotatedType of
      Just (TypeNum expectedType) ->
        case value of
            Lit (IntLit val mtype) -> -- Add support for new style int literals
                case mtype of
                    Just actualType ->
                        if actualType == expectedType
                            then return ()
                            else throwError $ UnexpectedToken
                                (Located (TNumber val) 0 0)
                                ("integer literal of type " ++ show expectedType)
                    Nothing -> return ()  -- Inferred type will be handled by convertToIR
            Lit (FloatLit val mtype) -> -- Add support for new style float literals
                case mtype of
                    Just actualType ->
                        if actualType == expectedType
                            then return ()
                            else throwError $ UnexpectedToken
                                (Located (TNumber val) 0 0)
                                ("float literal of type " ++ show expectedType)
                    Nothing -> return ()  -- Inferred type will be handled by convertToIR
            _ -> return ()  -- Allow other expressions for now
      _ -> return ()

    case locToken nameTok of
        TWord varName -> do
            traceM $ "Single binding line completed for: " ++ varName
            return (varName, value)
        _ -> throwError $ UnexpectedToken nameTok "identifier"

parseBlockImpl :: Parser Expr
parseBlockImpl = do
  st <- get
 
  _ <- matchToken isBlockKeyword "block"
  nameTok <- matchToken isValidName "block name"
  _ <- matchToken isColon ":"

  let blockName = case locToken nameTok of
        TWord name -> name
        _ -> error "Invalid block name"

  let currentIndent = stateIndent st
  let newIndent = currentIndent + 2
  let ctx = makeIndentContext BasicBlock newIndent currentIndent

  -- Set new indent context
  modify $ \s -> s { stateIndent = newIndent }

  -- Parse block contents with context
  (exprs, resultExpr) <- parseBlockContents ctx

  return $ Block $ BlockScope
    { blockLabel = blockName
    , blockExprs = exprs
    , blockResult = resultExpr
    }

-- Helper for block contents
parseBlockContents :: IndentContext -> Parser ([Expr], Maybe Expr)
parseBlockContents ctx = do
    modify $ \s -> s { stateIndent = baseIndent ctx }

    st <- get
    traceM $ "\n=== parseBlockContents ==="
    traceM $ "Context type: " ++ show ctx
    traceM $ "Current indent: " ++ show (stateIndent st)
    traceM $ "Current tokens: " ++ show (take 3 $ stateTokens st)

    case stateTokens st of
        [] -> return ([], Nothing)
        (tok:rest) -> case locToken tok of
            TEOF -> return ([], Nothing)
            _ -> do
                traceM $ "Block contents token: " ++ show tok
                traceM $ "Current indent context: base=" ++ show (baseIndent ctx)
                traceM $ "Current state indent: " ++ show (stateIndent st)
                traceM $ "Token column: " ++ show (locCol tok)
                traceM $ "Token line: " ++ show (locLine tok)

                -- Add line gap detection
                let nextLineNum = case rest of
                      (next:_) -> locLine next
                      [] -> locLine tok
                let hasLineGap = nextLineNum > locLine tok + 1

                traceM $ "Next line: " ++ show nextLineNum
                traceM $ "Line gap detected: " ++ show hasLineGap

                -- Regular indentation check
                when (isBlockStatement (locToken tok) && locCol tok < baseIndent ctx) $
                    throwError $ IndentationError $ IndentationErrorDetails (baseIndent ctx) (locCol tok) GreaterEq

                let shouldContinue = locCol tok >= baseIndent ctx
                if shouldContinue
                    then do
                        -- Parse current expression
                        expr <- case locToken tok of
                            TWord "print" -> parsePrintStatement
                            TWord "block" -> parseBlockImpl
                            _ -> parseExpression

                        -- Check if next token is dedented or after gap
                        case rest of
                            (next:_)
                                | hasLineGap && locCol next < baseIndent ctx ->
                                    -- This expr is last one - make it the result
                                    return ([], Just expr)
                                | locCol next < baseIndent ctx ->
                                    -- This expr is last one - make it the result
                                    return ([], Just expr)
                                | otherwise -> do
                                    -- More expressions follow
                                    (rest', result) <- parseBlockContents ctx
                                    return (expr:rest', result)
                            [] ->
                                -- Last expression becomes the result
                                return ([], Just expr)
                    else return ([], Nothing)

-- Helper to identify statements that must be properly indented in blocks
isBlockStatement :: Token -> Bool
isBlockStatement (TWord "print") = True
isBlockStatement (TWord "var") = True
isBlockStatement (TWord "let") = True
isBlockStatement _ = False

parseBlockExprs :: Maybe Type -> BlockType -> Int -> Parser ([Expr], Maybe Expr)
parseBlockExprs expectedType bt bi = do
    st <- get
    traceM $ "\n=== parseBlockExprs ==="
    traceM $ "Block type: " ++ show bt
    traceM $ "Initial tokens: " ++ show (take 3 $ stateTokens st)
    traceM $ "Base indent: " ++ show bi
    traceM $ "Current indent: " ++ show (stateIndent st)

    case stateTokens st of
        [] -> return ([], Nothing)
        (tok:_) -> do
            let tokCol = locCol tok
            traceM $ "Next token col: " ++ show tokCol
            traceM $ "Next token: " ++ show (locToken tok)

            -- Only check indentation if we're not at a block header
            let shouldCheckIndent = case locToken tok of
                  TWord "block" -> False
                  TWord "fn" -> False
                  _ -> True

            when shouldCheckIndent $ do
                traceM $ "Checking indentation: token col " ++ show tokCol ++ " against base " ++ show bi
                checkBlockIndent bt bi

            if locToken tok == TEOF
                then return ([], Nothing)
                else if tokCol < bi && shouldCheckIndent
                    then do
                        traceM $ "Token column " ++ show tokCol ++ " < base indent " ++ show bi
                        return ([], Nothing)
                    else case locToken tok of
                        TWord "print" -> do
                            traceM "Found print statement in block - handling specially"
                            printExpr <- parsePrintStatement
                            traceM $ "Parsed print statement: " ++ show printExpr
                            (moreExprs, resultExpr) <- parseBlockExprs expectedType bt bi
                            return (printExpr : moreExprs, resultExpr)

                        TWord "while" -> do
                            traceM "Found while statement in block"
                            currentIndent <- gets stateIndent  -- Save current indent
                            whileExpr <- parseWhileExpr expectedType
                            modify $ \s -> s { stateIndent = currentIndent }  -- Restore indent
                            (moreExprs, resultExpr) <- parseBlockExprs expectedType bt bi
                            return (whileExpr : moreExprs, resultExpr)

                        TWord "result" -> do
                            traceM "Parsing result expression"
                            resultExpr <- parseResultImpl
                            breaks <- parsePostResult bi
                            traceM $ "Result parsed with " ++ show (length breaks) ++ " following breaks"
                            return (breaks, Just resultExpr)

                        TWord "break" -> do
                            traceM "Parsing break statement"
                            breakExpr <- parseBreakImpl
                            (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
                            return (breakExpr : moreExprs, resultExpr)

                        TWord "var" -> do
                            traceM "Parsing variable declaration"
                            varExpr <- parseVarDecl
                            (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
                            return (varExpr : moreExprs, resultExpr)

                        TWord "block" -> do
                            traceM "Parsing block without wrapping"
                            expr <- parseBlockImpl  -- Direct parse
                            return ([expr], Nothing)  -- Don't wrap

                        TWord _ | isValidName (locToken tok) -> do
                            -- Look ahead for assignment
                            st' <- get
                            case drop 1 $ stateTokens st' of
                                (t:_) | locToken t == TEquals -> do
                                    assignExpr <- parseAssign expectedType
                                    (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
                                    return (assignExpr : moreExprs, resultExpr)
                                _ -> do
                                    expr <- parseExpressionWithType expectedType
                                    (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
                                    return (expr : moreExprs, resultExpr)

                        _ -> do
                            traceM "Parsing block expression"
                            expr <- parseExpressionWithType expectedType
                            (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
                            return (expr : moreExprs, resultExpr)

parseBreakImpl :: Parser Expr
parseBreakImpl = do
    st <- get
    traceM $ "parseBreak at indent " ++ show (stateIndent st)
    checkIndent GreaterEq
    _ <- matchToken isBreak "break"

    -- Look ahead for potential label
    st' <- get
    case stateTokens st' of
        (tok:_) | isValidName (locToken tok) -> do
            labelTok <- matchToken isValidName "block label"
            case locToken labelTok of
                TWord label -> return $ Break (Just label)
                _ -> error "Matched non-word token as break label"
        _ -> return $ Break Nothing  -- No label provided

parseResultImpl :: Parser Expr
parseResultImpl = do
    st <- get
    traceM $ "parseResult at indent " ++ show (stateIndent st)
    checkIndent GreaterEq
    _ <- matchToken isResult "result"
    traceM "Parsing result expression value"
    expr <- parseBasicExprImpl
    return $ Result expr

parsePostResult :: Int -> Parser [Expr]
parsePostResult bi = do
    st <- get
    traceM $ "parsePostResult at indent " ++ show bi
    case stateTokens st of
      [] -> return []
      (tok:_) -> do
        traceM $ "Post-result token: " ++ show tok
        if locToken tok == TEOF
            then return []
            else do
                when (locCol tok < bi) $
                    throwError $ IndentationError $ IndentationErrorDetails bi (locCol tok) GreaterEq
                case locToken tok of
                    TWord "break" -> do
                        traceM "Parsing post-result break statement"
                        breakExpr <- parseBreakImpl
                        rest <- parsePostResult bi
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
    st <- get

    case stateTokens st of
      (tok:_) -> do
        when (locCol tok < expectedIndent) $
          throwError $ IndentationError $ IndentationErrorDetails expectedIndent (locCol tok) GreaterEq
        _ <- matchToken isPrint "print"

        -- Parse the argument as a complete expression
        -- This change ensures we parse the entire expression including parentheses
        arg <- parseExpression
        traceM $ "Parsed print argument: " ++ show arg
        return $ Call "print" [arg]
      [] -> throwError $ EndOfInput "print"

parseWhileExpr :: Maybe Type -> Parser Expr
parseWhileExpr expectedType = do
    traceM "Parsing while expression"
    _ <- matchToken (\t -> t == TWord "while") "while keyword"
    condition <- parseExpression
    _ <- matchToken isColon ":"
    bodyState <- get
    traceM $ "While loop body state: " ++ show bodyState

    -- Get current parent block's base indent
    let parentIndent = stateIndent bodyState
    traceM $ "Parent indent level: " ++ show parentIndent

    -- Calculate child block indent relative to parent
    let blockIndent = parentIndent + 2
    traceM $ "Setting block indent to: " ++ show blockIndent

    -- Parse block contents at new indent level
    let savedIndent = stateIndent bodyState
    modify $ \s -> s { stateIndent = blockIndent }
    (bodyExprs, mResult) <- parseBlockExprs expectedType BasicBlock blockIndent
    modify $ \s -> s { stateIndent = savedIndent }  -- Restore parent indent

    let blockScope = BlockScope "while_body" bodyExprs mResult
    let body = Block blockScope
    traceM $ "Parsed while body: " ++ show body
    return $ While condition body

parseFuncDecl :: Parser Decl
parseFuncDecl = do
    traceM "\n=== parseFuncDecl ==="
    oldIndent <- gets stateIndent
    traceM $ "Starting indent: " ++ show oldIndent

    traceM "Parsing function declaration"
    _ <- matchToken (\t -> t == TWord "fn") "fn"
    nameTok <- matchToken isValidName "function name"
    _ <- matchToken (== TLeftParen) "("

    params <- parseParams

    _ <- matchToken (== TColon) ":"
    retTypeTok <- matchToken isValidName "return type"
    retType <- parseTypeToken retTypeTok

    _ <- matchToken (== TEquals) "="

    -- Create function context with standard indent
    let indent = 2
    let ctx = makeIndentContext FunctionBlock indent 0

    -- Parse first expression with indentation
    traceM "=== Parsing function body ==="
    st <- get

    case stateTokens st of
        (tok:_) -> do
            when (locCol tok < indent) $
                throwError $ IndentationError $ IndentationErrorDetails indent (locCol tok) GreaterEq

            expr <- parseExpression

            -- Check next token for line gap and dedent
            st' <- get
            case stateTokens st' of
                (next:_)
                    | locLine next > locLine tok + 1 && locCol next < indent -> do
                        traceM $ "Found line gap and dedent - ending function body"
                        modify $ \s -> s { stateIndent = 0 }  -- Reset indent for top level
                        let body = Block $ BlockScope "function_body" [expr] Nothing
                        case locToken nameTok of
                            TWord name -> return $ DFunc name params retType body
                            _ -> throwError $ UnexpectedToken nameTok "function name"
                    | otherwise -> do
                        -- Continue parsing block contents normally
                        (rest, result) <- parseBlockContents ctx
                        modify $ \s -> s { stateIndent = 0 }  -- Reset indent for top level
                        let body = Block $ BlockScope "function_body" (expr:rest) result
                        case locToken nameTok of
                            TWord name -> return $ DFunc name params retType body
                            _ -> throwError $ UnexpectedToken nameTok "function name"

                [] -> throwError $ EndOfInput "function body"

        [] -> throwError $ EndOfInput "function body"

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
                st <- get
                case stateTokens st of
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
    TWord "i64" -> return $ TypeNum Int64
    TWord "f32" -> return $ TypeNum Float32
    TWord "f64" -> return $ TypeNum Float64
    _ -> throwError $ UnexpectedToken tok "valid type"

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
