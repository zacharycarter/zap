{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Core
  ( ParseState(..)
  , ParseError(..)
  , Parser
  , ParserResult
  , checkIndent
  , matchToken
  , runParser
  ) where

import Control.Monad (unless)
import Control.Monad.State
import Control.Monad.Except
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

-- | Get the next token if it matches
matchToken :: (Token -> Bool) -> String -> Parser Located
matchToken pred expected = do
  state <- get
  case stateTokens state of
    [] -> throwError $ EndOfInput expected
    (tok:rest) ->
      if pred (locToken tok)
        then do
          put state { stateTokens = rest, stateCol = locCol tok }
          return tok
        else throwError $ UnexpectedToken tok expected
