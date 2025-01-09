

{-# LANGUAGE OverloadedStrings #-}

module Zap.Analysis.Lexical
  ( Token(..)
  , Located(..)
  , tokenize
  , LexError(..)
  , isAlpha
  ) where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Char (isAlphaNum, isDigit)
import Debug.Trace

-- Token definitions
data Token
  = TWord String        -- Identifiers and keywords
  | TString String      -- String literals
  | TNumber String      -- Numeric literals
  | TTypeSuffix String  -- Type suffix like 'f32, 'i32, etc...
  | TOperator String    -- Operators
  | TSymbol String      -- Other symbols
  | TColon             -- Block symbol
  | TVec String        -- Vector constructors
  | TDot               -- Struct field access
  | TComment String    -- Single-line comment
  | TType              -- Type keyword
  | TStruct            -- Struct keyword
  | TEOF               -- End of file
  | TLeftParen
  | TRightParen
  | TComma
  deriving (Show, Eq)

-- Located tokens
data Located = Located
  { locToken :: Token
  , locCol :: Int
  , locLine :: Int
  } deriving (Show, Eq)

-- Lexer error types
data LexError
  = UnterminatedString Int Int -- Line and column of the opening quote
  | InvalidCharacter Char Int Int -- Character, line, and column
  deriving (Show, Eq)

data LexerState = LexerState
  { inVectorLit :: Bool
  , lastToken :: String
  } deriving (Show, Eq)

initialLexerState :: LexerState
initialLexerState = LexerState False ""

-- Main tokenize function
tokenize :: Text -> Either LexError [Located]
tokenize input = scanTokens 1 1 (T.unpack input)

-- Token scanner with state
scanTokens :: Int -> Int -> String -> Either LexError [Located]
scanTokens = scanTokensWithState initialLexerState

scanTokensWithState :: LexerState -> Int -> Int -> String -> Either LexError [Located]
scanTokensWithState _state line col [] = do
    traceM $ "scanTokens: Reached end of input at line " ++ show line ++ ", col " ++ show col
    Right [Located TEOF col line]

scanTokensWithState state line col (c:cs) = do
    traceM $ "scanTokens: Processing character '" ++ [c] ++ "' at line " ++ show line ++ ", col " ++ show col
    case c of
        '#' -> do
            traceM $ "scanTokens: Found comment at line " ++ show line ++ ", col " ++ show col
            let (comment, rest) = span (/= '\n') cs
            traceM $ "scanTokens: Comment content: " ++ comment
            case rest of
                '\n':rs -> scanTokensWithState state (line + 1) 1 rs
                [] -> Right [Located TEOF col line]
                rs -> scanTokensWithState state line (col + length comment + 1) rs

        '(' -> do
            traceM $ "scanTokens: Found left paren at line " ++ show line ++ ", col " ++ show col
            let newState = state { inVectorLit = lastToken state `elem` ["Vec2", "Vec3", "Vec4"] }
            rest <- scanTokensWithState newState line (col + 1) cs
            Right (Located TLeftParen col line : rest)

        ')' -> do
            traceM $ "scanTokens: Found right paren at line " ++ show line ++ ", col " ++ show col
            let newState = state { inVectorLit = False }
            rest <- scanTokensWithState newState line (col + 1) cs
            Right (Located TRightParen col line : rest)

        -- ',' -> do
        --     traceM $ "Found comma at line " ++ show line ++ ", col " ++ show col
        --     traceM $ "Vector literal context: " ++ show (inVectorLit state)
        --     traceM $ "Next char: " ++ take 1 (show cs)

        --     let noSpaceAfterComma = null cs || not (isSpace (head cs))
        --     if inVectorLit state || noSpaceAfterComma
        --         then do
        --             traceM "Accepting comma"
        --             rest <- scanTokensWithState state line (col + 1) cs
        --             Right (Located TComma col line : rest)
        --         else do
        --             traceM "Rejecting comma - requires space after"
        --             throwError $ InvalidCharacter ',' line col


        ',' -> do
            traceM $ "Found comma at line " ++ show line ++ ", col " ++ show col
            let skipWhitespace = dropWhile isSpace cs
            case skipWhitespace of
                ',':_ -> do
                    let errorCol = col + (length $ takeWhile isSpace cs)
                    throwError $ InvalidCharacter ',' line errorCol
                _ -> do
                    rest <- scanTokensWithState state line (col + 1) cs
                    Right (Located TComma col line : rest)

        '.' -> do
            traceM $ "scanTokens: Found field access dot at line " ++ show line ++ ", col " ++ show col
            rest <- scanTokensWithState state line (col + 1) cs
            Right (Located TDot col line : rest)

        ':' -> do
            traceM $ "scanTokens: Found colon at line " ++ show line ++ ", col " ++ show col
            rest <- scanTokensWithState state line (col + 1) cs
            Right (Located TColon col line : rest)

        '"' -> do
            traceM $ "scanTokens: Starting string literal"
            lexString line col "" cs

        _ | isSpace c -> do
            traceM $ "scanTokens: Processing whitespace"
            case c of
                '\n' -> scanTokensWithState state (line + 1) 1 cs
                _ -> scanTokensWithState state line (col + 1) cs

        _ | isOperator c -> do
            traceM $ "scanTokens: Starting operator"
            lexOperator line col [c] cs state  -- Pass state to lexOperator

        _ | isDigit c -> do
            traceM $ "scanTokens: Starting number"
            lexNumber line col [c] cs

        _ | isAlpha c -> do
            traceM $ "scanTokens: Starting word"
            lexWord line col [c] cs

        _ -> Left $ InvalidCharacter c line col

lexOperator :: Int -> Int -> String -> String -> LexerState -> Either LexError [Located]
lexOperator line col acc cs state = do
    case cs of
        [] -> do
            let tok = case reverse acc of
                    "=" -> Located (TOperator "=") col line
                    "==" -> Located (TOperator "==") col line
                    "+=" -> Located (TOperator "+=") col line
                    op -> Located (TOperator op) col line
            Right [tok, Located TEOF (col + length acc) line]
        (c:rest)
            | isOperator c -> do
                -- Handle potential compound operator
                case (reverse acc, c) of
                    ("+", '=') -> lexOperator line col ('=':acc) rest state
                    _ -> lexOperator line col (c:acc) rest state
            | c == '(' && reverse acc `elem` ["Vec2", "Vec3", "Vec4"] -> do
                let newState = state { inVectorLit = True }
                rest' <- scanTokensWithState newState line (col + length acc + 1) rest
                let tok = Located (TVec (reverse acc)) col line
                Right (tok : Located TLeftParen (col + length acc) line : rest')
            | otherwise -> do
                let tok = case reverse acc of
                        "=" -> Located (TOperator "=") col line
                        "==" -> Located (TOperator "==") col line
                        "+=" -> Located (TOperator "+=") col line
                        op -> Located (TOperator op) col line
                rest' <- scanTokensWithState state line (col + length acc) (c:rest)
                Right (tok : rest')

isOperator :: Char -> Bool
isOperator c = c `elem` ("+-*/<>=&|" :: String)

-- Lexer for strings
lexString :: Int -> Int -> String -> String -> Either LexError [Located]
lexString line startCol _acc [] =
    Left $ UnterminatedString line startCol
lexString line startCol acc (c:cs) = case c of
    '"' -> do
        rest <- scanTokens line (startCol + length acc + 2) cs
        Right (Located (TString (reverse acc)) startCol line : rest)
    '\n' ->
        Left $ UnterminatedString line startCol
    _ ->
        lexString line startCol (c:acc) cs

-- Lexer for numbers
lexNumber :: Int -> Int -> String -> String -> Either LexError [Located]
lexNumber line col acc [] = do
    rest <- scanTokens line (col + length acc) []
    Right $ Located (TNumber (reverse acc)) col line : rest
lexNumber line col acc (c:cs) = case c of
    '\'' -> do  -- Found type suffix marker
        let (typeSuffix, rest) = span isValidTypeSuffix cs
        if not (null typeSuffix) && isValidTypeSpec typeSuffix
            then do
                let numTok = Located (TNumber (reverse acc)) col line
                let suffixTok = Located (TTypeSuffix typeSuffix) (col + length acc + 1) line
                rest' <- scanTokens line (col + length acc + length typeSuffix + 1) rest
                Right $ numTok : suffixTok : rest'
            else Left $ InvalidCharacter '\'' line col
    _ | isDigit c || c == '.' -> lexNumber line col (c:acc) cs
    _ -> do
        let numTok = Located (TNumber (reverse acc)) col line
        rest <- scanTokens line (col + length acc) (c:cs)
        Right $ numTok : rest

-- Helper to check valid type suffix characters
isValidTypeSuffix :: Char -> Bool
isValidTypeSuffix c = isAlphaNum c || c == '_'

-- Helper to validate complete type specifications
isValidTypeSpec :: String -> Bool
isValidTypeSpec s = s `elem` ["i32", "i64", "f32", "f64"]

-- Lexer for words
lexWord :: Int -> Int -> String -> String -> Either LexError [Located]
lexWord line col acc [] = do
    let tok = createWordToken (reverse acc) col line
    Right [tok, Located TEOF (col + length acc) line]

lexWord line col acc (c:cs)
    | isAlphaNum c || c == '_' = lexWord line col (c:acc) cs
    | otherwise = do
        let tok = createWordToken (reverse acc) col line
        rest <- scanTokens line (col + length acc) (c:cs)
        case rest of
            [] -> Right [tok, Located TEOF (col + length acc) line]
            rs -> Right (tok : rs)

createWordToken :: String -> Int -> Int -> Located
createWordToken word col line = Located token col line
  where
    token = case word of
      "Vec2" -> TVec "Vec2"
      "Vec3" -> TVec "Vec3"
      "Vec4" -> TVec "Vec4"
      "let"  -> TWord "let"
      "type" -> TType
      "struct" -> TStruct
      _      -> TWord word

-- Check for spaces
isSpace :: Char -> Bool
isSpace c = c `elem` [' ', '\t', '\n', '\r']

isAlpha :: Char -> Bool
isAlpha c = C.isAlpha c || c == '_'
