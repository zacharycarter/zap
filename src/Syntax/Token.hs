-- Syntax/Token.hs
module Syntax.Token
  ( Token(..)
  , TokenWithPos(..)
  ) where

import Text.Megaparsec.Pos (SourcePos)

data Token
  = TPrint
  | TNewline
  | TString String
  | TLet
  | TIdent String
  | TInt Int
  | TEqual
  | TColon
  | TIndent Int
  | TDedent Int
  | TPlus
  | TSemicolon
  | TBreak
  | TBlock
  | TTrue
  | TFalse
  | TIf
  | TThen
  | TElse
  | TSpace
  deriving (Show, Eq)

data TokenWithPos = TokenWithPos
  { tokenPos :: SourcePos
  , token    :: Token
  } deriving (Show, Eq)
