module Zap.Parser.Types where

-- | Indentation relations between nodes
data IndentRel
  = Equal     -- Child at same indent as parent (=)
  | Greater   -- Child at greater indent than parent (>)
  | GreaterEq -- Child at greater or equal indent (≥)
  | Any       -- No indent relation enforced (⊛)
  deriving (Show, Eq)

data BlockContext = BlockContext
  { blockIndent :: Int
  , parentIndent :: Int
  , isOutermostBlock :: Bool
  } deriving (Show, Eq)

-- | A column position in the source code
type Column = Int

-- | A terminal symbol with its column position
data Terminal = Terminal
  { termValue :: String  -- ^ The actual terminal text
  , termCol   :: Column  -- ^ The column where it appears
  } deriving (Show, Eq)

-- | A non-terminal indented node
data IndNode
  = IndNonTerm
      { nodeName :: String         -- ^ Name of the non-terminal
      , nodeCol  :: Column         -- ^ Column position
      , children :: [(IndentRel, IndNode)]  -- ^ Child nodes with their relation to parent
      }
  | IndTerm Terminal
  deriving (Show, Eq)

-- | Productions in the grammar
data Production
  = Prod
      { prodName :: String               -- ^ Name of non-terminal being defined
      , prodRhs  :: [(IndentRel, String)]  -- ^ Right-hand side with indent relations
      }
  deriving (Show, Eq)

-- | A complete grammar
data Grammar = Grammar
  { productions :: [Production]
  , startSymbol :: String
  } deriving (Show, Eq)
