{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Program
  ( parseProgram
  ) where

import Control.Monad.State
import Data.Text (Text)
import Zap.Analysis.Lexical
import Zap.Parser.Types
import Zap.Parser.Core
import Zap.Parser.Expr

-- | Convert lexer errors into parser errors
mapLexError :: Either LexError a -> Either ParseError a
mapLexError (Left (UnterminatedString line col)) =
  Left $ EndOfInput $ "Unterminated string at line " ++ show line ++ ", column " ++ show col
mapLexError (Left (InvalidCharacter c line col)) =
  Left $ EndOfInput $ "Invalid character '" ++ [c] ++ "' at line " ++ show line ++ ", column " ++ show col
mapLexError (Right a) = Right a

-- | Parse a complete program
parseProgram :: Text -> Either ParseError (Located, Located)
parseProgram input = do
  tokens <- mapLexError $ tokenize input
  runParser parsePrint tokens
