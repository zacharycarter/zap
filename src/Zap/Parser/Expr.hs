{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Expr
  ( defaultExprParser
  , isPrint
  , isStringLit
  , parseExpr
  , parsePrint
  , ExprParser(..)
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
isValidName (TWord name) = traceShow ("Validating name: " ++ name) $
  not $ name `elem` ["print", "break", "result", "block"]
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

-- Main expression parser
parseExpr :: ExprParser -> Parser Expr
parseExpr parsers = do
    state <- get
    traceM $ "parseExpr state: " ++ show (stateIndent state) ++ " tokens: " ++ show (take 3 $ stateTokens state)
    case stateTokens state of
      (tok:_) -> do
        traceM $ "Parsing expression starting with: " ++ show (locToken tok)
        case locToken tok of
          TWord "block" -> parseBlock parsers
          TWord "break" -> parseBreak parsers
          TWord "result" -> parseResult parsers
          _ -> parseBasicExpr parsers
      [] -> throwError $ EndOfInput "expression"

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
        (tok:_) -> do
            -- **Added Check for TEOF**
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

-- parseBlockExprs :: Int -> Parser ([Expr], Maybe Expr)
-- parseBlockExprs baseIndent = do
--     state <- get
--     traceM $ "parseBlockExprs at indent " ++ show baseIndent ++ " with tokens: " ++ show (take 3 $ stateTokens state)
--     case stateTokens state of
--       [] -> return ([], Nothing)
--       (tok:_) -> do
--         traceM $ "Checking token: " ++ show tok ++ " at col " ++ show (locCol tok)
--         when (locCol tok < baseIndent) $
--           throwError $ IndentationError baseIndent (locCol tok) GreaterEq
--         case locToken tok of
--             TWord "result" -> do
--               traceM "Parsing result expression"
--               resultExpr <- parseResultImpl
--               breaks <- parsePostResult baseIndent
--               traceM $ "Result parsed with " ++ show (length breaks) ++ " following breaks"
--               return (breaks, Just resultExpr)
--             TWord "break" -> do
--               traceM "Parsing break statement"
--               breakExpr <- parseBreakImpl
--               (moreExprs, resultExpr) <- parseBlockExprs baseIndent
--               return (breakExpr : moreExprs, resultExpr)
--             TWord "block" -> do
--               traceM "Parsing nested block"
--               blockExpr <- parseBlockImpl
--               (moreExprs, resultExpr) <- parseBlockExprs baseIndent
--               return (blockExpr : moreExprs, resultExpr)
--             _ -> do
--               traceM "Parsing basic expression"
--               expr <- parseBasicExprImpl
--               (moreExprs, resultExpr) <- parseBlockExprs baseIndent
--               return (expr : moreExprs, resultExpr)

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
        -- Allow TEOF as a valid termination after result
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

-- parsePostResult :: Int -> Parser [Expr]
-- parsePostResult baseIndent = do
--     state <- get
--     traceM $ "parsePostResult at indent " ++ show baseIndent
--     case stateTokens state of
--       [] -> return []
--       (tok:_) -> do
--         traceM $ "Post-result token: " ++ show tok
--         when (locCol tok < baseIndent) $
--           throwError $ IndentationError baseIndent (locCol tok) GreaterEq
--         case locToken tok of
--             TWord "break" -> do
--               traceM "Parsing post-result break statement"
--               breakExpr <- parseBreakImpl
--               rest <- parsePostResult baseIndent
--               return (breakExpr : rest)
--             TWord "result" ->
--               throwError $ UnexpectedToken tok "only break statements allowed after result"
--             _ -> throwError $ UnexpectedToken tok "only break statements allowed after result"

parseBasicExprImpl :: Parser Expr
parseBasicExprImpl = do
    state <- get
    let tok = head $ stateTokens state
    traceM $ "parseBasicExpr at indent " ++ show (stateIndent state) ++ " with token: " ++ show tok
    checkIndent GreaterEq
    case locToken tok of
        TWord "print" -> do
            -- Handle print statement
            printTok <- matchToken isPrint "print"
            strTok <- matchToken isStringLit "string literal"
            case locToken strTok of
                TString s -> return $ Print (StrLit s)
                _ -> error "Matched non-string token as string literal"
        TString s -> do
            -- Handle string literal
            strTok <- matchToken isStringLit "string literal"
            return $ StrLit s
        _ -> throwError $ UnexpectedToken tok "expression"

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

defaultExprParser :: ExprParser
defaultExprParser = ExprParser
  { parseBasicExpr = parseBasicExprImpl
  , parseBlock = parseBlockImpl
  , parseBreak = parseBreakImpl
  , parseResult = parseResultImpl
  }
