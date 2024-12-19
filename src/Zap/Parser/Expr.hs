{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Expr
  ( ExprParser(..)
  , defaultExprParser
  , parseExpr
  , parseExpression
  , parseLetBinding
  , parsePrint
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
  not $ name `elem` ["print", "break", "result", "block", "let"]
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
isOperator (TOperator op) = op `elem` ["+","-","*","/","<", ">", "=", "&", "|"]
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
    parseAdditive

-- Parse addition and subtraction with proper precedence
parseAdditive :: Parser Expr
parseAdditive = do
    traceM "Parsing additive expression"
    left <- parseMultiplicative
    remainingAdditive left
  where
    remainingAdditive left = do
      state <- get
      case stateTokens state of
        (tok:_) -> case locToken tok of
          TOperator "+" -> do
            traceM "Found addition operator"
            _ <- matchToken isOperator "+"
            right <- parseMultiplicative
            remainingAdditive (BinOp Add left right)
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
    baseExpr <- parseBaseTerm
    parseFieldAccess baseExpr

parseBaseTerm :: Parser Expr
parseBaseTerm = do
    traceM "Parsing base term"
    state <- get
    case stateTokens state of
        (tok:rest) -> case locToken tok of
            TNumber n -> do
                _ <- matchToken isNumber "number"
                return $ NumLit Float32 n

            TWord name | isValidName (locToken tok) -> do
                traceM $ "Found identifier: " ++ name
                _ <- matchToken isValidName "identifier"
                case rest of
                    (nextTok:_) | isNumber (locToken nextTok) -> do
                        traceM "Found struct instantiation"
                        args <- parseStructArgs
                        return $ StructLit name args
                    _ -> do
                        traceM "Found variable reference"
                        return $ Var name

            TVec vecType -> do
                traceM $ "Found vector constructor"
                _ <- matchToken isVecConstructor "vector constructor"
                args <- parseVectorArgs vecType
                return $ VecLit (vecTypeFromString vecType) args

            _ -> throwError $ UnexpectedToken tok "term"
        [] -> throwError $ EndOfInput "term"

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

parseStructArgs :: Parser [(String, Expr)]
parseStructArgs = do
    traceM "Parsing struct arguments"
    -- For now, assume fields are in order defined in struct
    args <- parseExprList
    return $ zip ["x", "y"] args  -- Temporarily hardcoded field names

parseExprList :: Parser [Expr]
parseExprList = do
    traceM "Parsing expression list"
    let loop acc = do
          state <- get
          case stateTokens state of
            (tok:_) | isNumber (locToken tok) || isValidName (locToken tok) -> do
                        expr <- parseExpression
                        loop (expr : acc)
            _ -> return $ reverse acc
    loop []

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
            TVec vecType -> do
                traceM $ "Parsing vector constructor: " ++ vecType
                _ <- matchToken isVecConstructor "vector constructor"
                args <- parseVectorArgs vecType
                return $ VecLit (vecTypeFromString vecType) args
            TWord "print" -> do
                _ <- matchToken isPrint "print"
                expr <- parseBasicExprImpl
                return $ Print expr
            TWord name | isValidName (locToken tok) -> do
                _ <- matchToken isValidName "identifier"
                return $ Var name
            _ -> throwError $ UnexpectedToken tok "term"

parseLetBinding :: ExprParser -> Parser Expr
parseLetBinding parsers = do
    traceM "Parsing let binding"
    _ <- matchToken (\t -> t == TWord "let") "let keyword"
    name <- matchToken isValidName "identifier"
    traceM $ "Let binding name: " ++ show name
    _ <- matchToken (\t -> t == TEquals) "equals sign"
    traceM "Found equals sign, parsing value"
    value <- parseExpression
    case locToken name of
        TWord varName -> do
            traceM $ "Completed let binding for: " ++ varName
            return $ Let varName value
        _ -> throwError $ UnexpectedToken name "identifier"

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
                        _ -> do
                            traceM "Parsing basic expression"
                            expr <- parseBasicExprImpl
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
        TVec vecType -> do
            traceM $ "Parsing vector constructor: " ++ show vecType
            args <- parseVectorArgs vecType
            return $ VecLit (vecTypeFromString vecType) args
        _ -> throwError $ UnexpectedToken constructorTok "vector constructor"

parseVectorArgs :: String -> Parser [Expr]
parseVectorArgs vecType = do
    traceM $ "Parsing arguments for vector type: " ++ vecType
    let numArgs = case vecType of
            "Vec2" -> 2
            "Vec3" -> 3
            "Vec4" -> 4
            _ -> error "Invalid vector type"
    replicateM numArgs $ do
        tok <- matchToken isNumber "number"
        case locToken tok of
            TNumber n -> return $ NumLit Float32 n
            _ -> throwError $ UnexpectedToken tok "number"

parsePrintStatement :: Parser Expr
parsePrintStatement = do
    traceM "Parsing print statement"
    expectedIndent <- gets stateIndent
    state <- get
    let tok = head $ stateTokens state
    when (locCol tok < expectedIndent) $
        throwError $ IndentationError expectedIndent (locCol tok) GreaterEq
    _ <- matchToken isPrint "print"
    expr <- parseExpression  -- Use the full expression parser
    return $ Print expr

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

parsePrint :: Parser (Located, Located)
parsePrint = do
    state <- get
    traceM $ "parsePrint state: " ++ show state
    case stateTokens state of
      (tok:_) -> do
        traceM $ "Print token: " ++ show tok
        when (locCol tok /= 1) $
          throwError $ IndentationError 1 (locCol tok) Equal
        printTok <- matchToken isPrint "print"
        let printLine = locLine printTok
        modify (\s -> s { stateIndent = locCol printTok + 1 })
        traceM $ "Print statement indent set to: " ++ show (locCol printTok + 1)
        exprTok <- matchToken isStringLit "string literal"
        traceM $ "Print expression: " ++ show exprTok
        when (locLine exprTok > printLine && locCol exprTok <= locCol printTok) $ do
          traceM $ "Print indentation error: " ++ show exprTok
          throwError $ IndentationError (locCol printTok + 1) (locCol exprTok) Greater
        return (printTok, exprTok)
      [] -> throwError $ EndOfInput "print statement"
