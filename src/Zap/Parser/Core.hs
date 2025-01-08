{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Core
  ( ParseState(..)
  , ParseError(..)
  , Parser
  , ParserResult
  , IndentationErrorDetails(..)
  , checkIndent
  , checkBlockIndent
  , makeIndentContext
  , validateIndent
  , matchToken
  , runParser
  ) where

import Control.Monad (unless, when)
import Control.Monad.State
import Control.Monad.Except
import Debug.Trace

import Zap.Analysis.Lexical
import Zap.Parser.Types

-- | State needed during parsing
data ParseState = ParseState
  { stateTokens :: [Located]    -- Remaining tokens to parse
  , stateIndent :: Int         -- Current indentation level
  , stateCol    :: Int         -- Current column position
  } deriving (Show)

-- | Possible parsing errors
data IndentationErrorDetails = IndentationErrorDetails
  { expectedCol :: Int
  , actualCol :: Int
  , relation :: IndentRel
  } deriving (Show, Eq)

data ParseError
  = UnexpectedToken Located String  -- Got token, expected description
  | IndentationError IndentationErrorDetails
  | EndOfInput String  -- Expected description
  deriving (Show, Eq)

-- | The parser monad
type Parser a = StateT ParseState (Either ParseError) a

type ParserResult a = Either ParseError a


-- | Run a parser on input tokens
runParser :: Parser a -> [Located] -> ParserResult a
runParser p tokens = evalStateT p (ParseState tokens 0 0)

-- Basic combinators

-- | Check indentation against current level using given relation
checkIndent :: IndentRel -> Parser ()
checkIndent rel = do
  st <- get
  case stateTokens st of
    (tok:_) -> do
      let curIndent = stateIndent st
      let tokCol = locCol tok
      case rel of
        Equal     -> unless (tokCol == curIndent) $
                      throwError $ IndentationError $ IndentationErrorDetails curIndent tokCol rel
        Greater   -> unless (tokCol > curIndent) $
                      throwError $ IndentationError $ IndentationErrorDetails curIndent tokCol rel
        GreaterEq -> unless (tokCol >= curIndent) $
                      throwError $ IndentationError $ IndentationErrorDetails curIndent tokCol rel
        Any       -> return ()
    [] -> throwError $ EndOfInput "Expected token for indentation check"

-- Like checkIndent but takes BlockType
checkBlockIndent :: BlockType -> Int -> Parser ()
checkBlockIndent bt bi = do
    st <- get
    case stateTokens st of
        (tok:_) -> do
            let tokCol = locCol tok
            traceM $ "=== checkBlockIndent ==="
            traceM $ "Block type: " ++ show bt
            traceM $ "Base indent: " ++ show bi
            traceM $ "Token column: " ++ show tokCol
            traceM $ "Token: " ++ show (locToken tok)
            case locToken tok of
                TEOF -> return ()
                _ -> case bt of
                    TopLevel -> return ()
                    BasicBlock -> do
                        traceM $ "Checking BasicBlock indent: " ++ show tokCol ++ " vs " ++ show bi
                        if tokCol < bi
                            then do
                                -- Found dedent - this is valid block termination
                                traceM $ "Found block termination dedent"
                                modify $ \s -> s { stateIndent = tokCol }  -- Set new base indent
                                return ()
                            else return ()
                    FunctionBlock -> do
                        when (tokCol > 1 && tokCol < bi) $
                            throwError $ IndentationError $ IndentationErrorDetails bi tokCol GreaterEq
        [] -> return ()

-- | Get the next token if it matches
matchToken :: (Token -> Bool) -> String -> Parser Located
matchToken p expected = do
  st <- get
  traceM $ "Parser state before token match check: " ++ show (take 3 $ stateTokens st)
  case stateTokens st of
    [] -> throwError $ EndOfInput expected
    (tok:rest) ->
      if p (locToken tok)
        then do
          put st { stateTokens = rest, stateCol = locCol tok }
          return tok
        else throwError $ UnexpectedToken tok expected

-- Helper to create indent context
makeIndentContext :: BlockType -> Int -> Int -> IndentContext
makeIndentContext btype base parent = IndentContext
  { baseIndent = base
  , parentIndent = parent
  , blockType = btype
  }

-- Validates indentation relative to context
validateIndent :: IndentContext -> Int -> Parser ()
validateIndent ctx col = do
  case blockType ctx of
    BasicBlock ->
      when (col <= parentIndent ctx) $
        throwError $ IndentationError $ IndentationErrorDetails (baseIndent ctx) col GreaterEq

    FunctionBlock ->
      unless (col > baseIndent ctx) $
        throwError $ IndentationError $ IndentationErrorDetails (baseIndent ctx) col Greater

    TopLevel -> return () -- No indentation rules at top level
