{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Core
  ( ParseState(..)
  , ParseError(..)
  , Parser
  , ParserResult
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
data ParseError
  = UnexpectedToken Located String  -- Got token, expected description
  | IndentationError
      { expectedCol :: Int
      , actualCol :: Int
      , relation :: IndentRel
      }
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
  state <- get
  case stateTokens state of
    (tok:_) -> do
      let curIndent = stateIndent state
      let tokCol = locCol tok
      case rel of
        Equal     -> unless (tokCol == curIndent) $
                      throwError $ IndentationError curIndent tokCol rel
        Greater   -> unless (tokCol > curIndent) $
                      throwError $ IndentationError curIndent tokCol rel
        GreaterEq -> unless (tokCol >= curIndent) $
                      throwError $ IndentationError curIndent tokCol rel
        Any       -> return ()
    [] -> throwError $ EndOfInput "Expected token for indentation check"

-- Like checkIndent but takes BlockType
checkBlockIndent :: BlockType -> Int -> Parser ()
checkBlockIndent blockType baseIndent = do
    state <- get
    case stateTokens state of
        (tok:_) -> do
            let tokCol = locCol tok
            traceM $ "Checking " ++ show blockType ++
                    " indent: token col " ++ show tokCol ++
                    " against base " ++ show baseIndent
            case blockType of
                TopLevel -> return ()
                BasicBlock -> when (tokCol < baseIndent) $
                    throwError $ IndentationError baseIndent tokCol GreaterEq
                FunctionBlock -> do
                    -- Special case - allow dedenting to top level after function
                    when (tokCol > 1 && tokCol < baseIndent) $
                        throwError $ IndentationError baseIndent tokCol GreaterEq
        [] -> return ()

-- | Get the next token if it matches
matchToken :: (Token -> Bool) -> String -> Parser Located
matchToken pred expected = do
  state <- get
  traceM $ "Parser state before token match check: " ++ show (take 3 $ stateTokens state)
  case stateTokens state of
    [] -> throwError $ EndOfInput expected
    (tok:rest) ->
      if pred (locToken tok)
        then do
          put state { stateTokens = rest, stateCol = locCol tok }
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
        throwError $ IndentationError (baseIndent ctx) col GreaterEq

    FunctionBlock ->
      unless (col > baseIndent ctx) $
        throwError $ IndentationError (baseIndent ctx) col Greater

    TopLevel -> return () -- No indentation rules at top level
