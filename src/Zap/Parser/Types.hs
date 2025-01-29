module Zap.Parser.Types
  ( IndentRel (..),
    BlockType (..),
    IndentContext (..),
  )
where

-- | Indentation relations between nodes
data IndentRel
  = Equal -- Child at same indent as parent (=)
  | Greater -- Child at greater indent than parent (>)
  | GreaterEq -- Child at greater or equal indent (≥)
  | Any -- No indent relation enforced (⊛)
  deriving (Show, Eq)

data BlockType
  = TopLevel
  | BasicBlock -- For blocks like "block test:"
  | FunctionBlock
  | TypeBlock
  deriving (Show, Eq)

data IndentContext = IndentContext
  { baseIndent :: Int,
    parentIndent :: Int,
    blockType :: BlockType
  }
  deriving (Show, Eq)
