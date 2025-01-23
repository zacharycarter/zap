module Zap.Parser.Types
  ( IndentRel (..),
    BlockType (..),
    IndentContext (..),
    BlockContext (..),
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

data BlockContext = BlockContext
  { blockIndent :: Int,
    parentBlockIndent :: Int,
    isOutermostBlock :: Bool
  }
  deriving (Show, Eq)

-- | A column position in the source code
type Column = Int

-- | A terminal symbol with its column position
data Terminal = Terminal
  { -- | The actual terminal text
    termValue :: String,
    -- | The column where it appears
    termCol :: Column
  }
  deriving (Show, Eq)

-- | A non-terminal node
data NonTerminal = NonTerminal
  { -- | Name of the non-terminal node
    nodeName :: String,
    -- | Column position
    nodeCol :: Column,
    -- | Child nodes with their relation to parent
    children :: [(IndentRel, IndNode)]
  }
  deriving (Show, Eq)

-- | An indented node
data IndNode
  = IndNonTerm NonTerminal
  | IndTerm Terminal
  deriving (Show, Eq)

-- | Productions in the grammar
data Production = Prod
  { -- | Name of non-terminal being defined
    prodName :: String,
    -- | Right-hand side with indent relations
    prodRhs :: [(IndentRel, String)]
  }
  deriving (Show, Eq)

-- | A complete grammar
data Grammar = Grammar
  { productions :: [Production],
    startSymbol :: String
  }
  deriving (Show, Eq)
