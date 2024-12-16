{-# LANGUAGE OverloadedStrings #-}

module Zap.Analysis.Lexical
  ( Token(..)
  , Located(..)
  , tokenize
  , LexError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Debug.Trace

-- Token definitions
data Token
  = TWord String        -- Identifiers like 'print', 'block', 'break'
  | TString String      -- String literals like "Hello, World!"
  | TNumber String      -- Numeric literals like 123
  | TOperator String    -- Operators like '+', '-', '*'
  | TSymbol String      -- Other symbols
  | TColon             -- Explicit block symbol ':'
  | TEOF               -- End of file marker
  deriving (Show, Eq)

-- Located tokens
data Located = Located
  { locToken :: Token
  , locCol :: Int  -- Column number (1-based)
  , locLine :: Int -- Line number (1-based)
  } deriving (Show, Eq)

-- Lexer error types
data LexError
  = UnterminatedString Int Int -- Line and column of the opening quote
  | InvalidCharacter Char Int Int -- Character, line, and column
  deriving (Show, Eq)

-- Main tokenize function
tokenize :: Text -> Either LexError [Located]
tokenize input = scanTokens 1 1 (T.unpack input)

-- Token scanner
scanTokens :: Int -> Int -> String -> Either LexError [Located]
scanTokens line col [] = Right [Located TEOF col line]
scanTokens line col (c:cs)
  | c == '"' = lexString line col [] cs
  | c == ':' = do
      rest <- scanTokens line (col + 1) cs
      Right $ Located TColon col line : rest
  | isSpace c = case c of
      '\n' -> scanTokens (line + 1) 1 cs
      _    -> scanTokens line (col + 1) cs
  | isDigit c = lexNumber line col [c] cs
  | isAlpha c = lexWord line col [c] cs
  | isOperator c = lexOperator line col [c] cs
  | otherwise = Left $ InvalidCharacter c line col

-- Lexer for operators
lexOperator :: Int -> Int -> String -> String -> Either LexError [Located]
lexOperator line col acc [] = do
    traceM $ "lexOperator: End of input with accumulated: " ++ show acc
    let tok = Located (TOperator (reverse acc)) col line
    traceM $ "lexOperator: Created operator token: " ++ show tok
    rest <- scanTokens line (col + length acc) []
    traceM $ "lexOperator: Rest of tokens after EOF handling: " ++ show rest
    Right (tok : rest)
lexOperator line col acc (c:cs)
    | isOperator c = do
        traceM $ "lexOperator: Found operator char: " ++ [c] ++ " with accumulated: " ++ show acc
        lexOperator line col (c:acc) cs
    | otherwise = do
        traceM $ "lexOperator: Non-operator char: " ++ [c] ++ " with accumulated: " ++ show acc
        let tok = Located (TOperator (reverse acc)) col line
        traceM $ "lexOperator: Created operator token: " ++ show tok
        rest <- scanTokens line (col + length acc) (c:cs)
        traceM $ "lexOperator: Rest of tokens: " ++ show rest
        Right (tok : rest)

isOperator :: Char -> Bool
isOperator c = c `elem` ("+-*/<>=&|" :: String)

-- Lexer for strings
lexString :: Int -> Int -> String -> String -> Either LexError [Located]
lexString line startCol acc [] = Left $ UnterminatedString line startCol
lexString line startCol acc (c:cs) = case c of
  '"' -> do
    rest <- scanTokens line (startCol + length acc + 2) cs
    let tok = Located (TString (reverse acc)) startCol line
    Right (tok : rest)
  '\n' -> Left $ UnterminatedString line startCol
  _ -> lexString line startCol (c:acc) cs

-- Lexer for numbers
lexNumber :: Int -> Int -> String -> String -> Either LexError [Located]
lexNumber line col acc [] = do
  let tok = Located (TNumber (reverse acc)) col line
  Right [tok]
lexNumber line col acc (c:cs)
  | isDigit c = lexNumber line col (c:acc) cs
  | otherwise = do
      let tok = Located (TNumber (reverse acc)) col line
      rest <- scanTokens line (col + length acc) (c:cs)
      Right (tok : rest)

-- Lexer for words
lexWord :: Int -> Int -> String -> String -> Either LexError [Located]
lexWord line col acc [] = do
    traceM $ "lexWord: End of input with accumulated: " ++ show acc
    let tok = Located (TWord (reverse acc)) col line
    rest <- scanTokens line (col + length acc) []
    traceM $ "lexWord: Rest of tokens after EOF handling: " ++ show rest
    Right (tok : rest)
lexWord line col acc (c:cs)
    | isAlphaNum c = lexWord line col (c:acc) cs
    | otherwise = do
        traceM $ "lexWord: Non-alphanumeric char: " ++ [c] ++ " with accumulated: " ++ show acc
        let tok = Located (TWord (reverse acc)) col line
        rest <- scanTokens line (col + length acc) (c:cs)
        Right (tok : rest)

-- Check for spaces
isSpace :: Char -> Bool
isSpace c = c `elem` [' ', '\t', '\n', '\r']
