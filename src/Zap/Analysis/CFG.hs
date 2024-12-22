{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.CFG
  ( CFG(..)
  , BasicBlock(..)
  , CFGNode(..)
  , buildCFG
  , CFGError(..)
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except

import Zap.IR.Core

-- | A Control Flow Graph node identifier
type NodeId = Int

-- | A basic block in the CFG
data BasicBlock = BasicBlock
  { blockId :: NodeId
  , blockStmts :: [IRExpr]
  , blockTerminator :: Maybe IRExpr
  } deriving (Show, Eq)

-- | A node in the control flow graph
data CFGNode = CFGNode
  { nodeBlock :: BasicBlock
  , successors :: S.Set NodeId
  , predecessors :: S.Set NodeId
  } deriving (Show, Eq)

-- | The Control Flow Graph itself
data CFG = CFG
  { cfgNodes :: M.Map NodeId CFGNode
  , cfgEntry :: NodeId
  , cfgExit :: NodeId
  } deriving (Show, Eq)

-- | Errors that can occur during CFG construction
data CFGError
  = EmptyProgram
  | InvalidBlock
  deriving (Show, Eq)

-- | State maintained during CFG construction
data CFGState = CFGState
  { nextNodeId :: NodeId
  , currentBlock :: Maybe BasicBlock
  , builtNodes :: M.Map NodeId CFGNode
  } deriving (Show)

type CFGBuilder = StateT CFGState (Except CFGError)

-- | Build a CFG from IR expressions
buildCFG :: [IRExpr] -> Either CFGError CFG
buildCFG [] = Left EmptyProgram
buildCFG exprs = do
  (nodes, entry) <- runExcept $ evalStateT (buildBlocks exprs) initialState
  let exit = maximum $ M.keys nodes
  return $ CFG
    { cfgNodes = nodes
    , cfgEntry = entry
    , cfgExit = exit
    }
  where
    initialState = CFGState
      { nextNodeId = 0
      , currentBlock = Nothing
      , builtNodes = M.empty
      }

-- | Build basic blocks from IR expressions
buildBlocks :: [IRExpr] -> CFGBuilder (M.Map NodeId CFGNode, NodeId)
buildBlocks exprs = do
  entryId <- freshNodeId
  let entryBlock = BasicBlock entryId exprs Nothing
  modify $ \s -> s { currentBlock = Just entryBlock }
  finalizeCurrentBlock
  nodes <- gets builtNodes
  return (nodes, entryId)

-- | Get a fresh node identifier
freshNodeId :: CFGBuilder NodeId
freshNodeId = do
  id <- gets nextNodeId
  modify $ \s -> s { nextNodeId = id + 1 }
  return id

-- | Finalize the current basic block and add it to the graph
finalizeCurrentBlock :: CFGBuilder ()
finalizeCurrentBlock = do
  mblock <- gets currentBlock
  case mblock of
    Nothing -> return ()
    Just block -> do
      let node = CFGNode
            { nodeBlock = block
            , successors = S.empty
            , predecessors = S.empty
            }
      modify $ \s -> s
        { builtNodes = M.insert (blockId block) node (builtNodes s)
        , currentBlock = Nothing
        }
