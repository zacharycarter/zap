{-# LANGUAGE OverloadedStrings #-}

module Syntax.Lexer
    ( Token(..)
    , lexProgram
    ) where

import Text.Megaparsec hiding(Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text, pack)
import Control.Monad (void)
import Data.Functor (($>))
import Data.List (foldl')

-- Define your Token data type
data Token
    = TPrint
    | TLet
    | TIf
    | TThen
    | TElse
    | TBreak
    | TBlock
    | TTrue
    | TFalse
    | TIdent String
    | TString String
    | TInt Int
    | TPlus
    | TEqual
    | TLParen
    | TRParen
    | TColon
    | TNewline
    | TIndent Int
    | TDedent Int
    deriving (Show, Eq, Ord)

type Parser = Parsec Void Text

-- List of reserved keywords and their corresponding tokens
reservedWords :: [(Text, Token)]
reservedWords =
    [ ("print", TPrint)
    , ("let", TLet)
    , ("if", TIf)
    , ("then", TThen)
    , ("else", TElse)
    , ("break", TBreak)
    , ("block", TBlock)
    , ("true", TTrue)
    , ("false", TFalse)
    ]

-- Space consumer that handles spaces, tabs, newlines, and comments
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "#") empty

-- Consumes spaces and tabs but not newlines
sc :: Parser ()
sc = L.space (void $ oneOf [' ', '\t']) (L.skipLineComment "#") empty

-- Parses lexemes by consuming trailing space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parses specific symbols and consumes trailing space
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Parses reserved keywords, ensuring they are not followed by alphanumerics
reserved :: Text -> Token -> Parser Token
reserved name tok = try (string name *> notFollowedBy alphaNumChar *> sc *> pure tok)

-- Parses identifiers, distinguishing them from reserved keywords
identifier :: Parser Token
identifier = lexeme $ do
    first <- letterChar
    rest <- many alphaNumChar
    let word = first : rest
    case lookup (pack word) reservedWords of
        Just tok -> return tok
        Nothing  -> return $ TIdent word

-- Parses string literals enclosed in double quotes
stringLiteral :: Parser Token
stringLiteral = TString <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

-- Parses integer literals
integer :: Parser Token
integer = TInt <$> lexeme L.decimal

-- Parses operators and punctuation
operator :: Parser Token
operator = choice
    [ TPlus  <$ symbol "+"
    , TEqual <$ symbol "="
    , TColon <$ symbol ":"
    , TLParen <$ symbol "("
    , TRParen <$ symbol ")"
    ]

-- Parses newlines
newlineToken :: Parser Token
newlineToken = TNewline <$ char '\n'

-- The main lexer parser that produces a list of tokens
lexProgram :: Parser [Token]
lexProgram = do
    tokens <- many ( choice [reservedParser, operator, integer, stringLiteral, identifier, newlineToken] )
    eof
    return tokens
    where
        reservedParser = choice $ map (\(kw, tok) -> reserved kw tok) reservedWords
