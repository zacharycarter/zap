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
import Debug.Trace

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

-- | Edge definition for delayed edge creation
data Edge = Edge
  { fromNode :: NodeId
  , toNode :: NodeId
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
  , pendingEdges :: [Edge]  -- Collect edges for delayed creation
  } deriving (Show)

type CFGBuilder = StateT CFGState (Except CFGError)

initialState :: CFGState
initialState = CFGState
  { nextNodeId = 0
  , currentBlock = Nothing
  , builtNodes = M.empty
  , pendingEdges = []
  }

-- | Build a CFG from IR expressions
buildCFG :: [IRExpr] -> Either CFGError CFG
buildCFG [] = Left EmptyProgram
buildCFG exprs = runExcept $ do
  -- First phase: Create all blocks
  (nodes, edges) <- evalStateT (createBlocks exprs) initialState

  -- Second phase: Add all edges
  let withEdges = addEdgesToGraph nodes edges

  -- Get entry and exit nodes
  let entry = minimum $ M.keys nodes
      exit = maximum $ M.keys nodes

  return $ CFG
    { cfgNodes = withEdges
    , cfgEntry = entry
    , cfgExit = exit
    }

-- | Create all blocks without edges
createBlocks :: [IRExpr] -> CFGBuilder (M.Map NodeId CFGNode, [Edge])
createBlocks exprs = do
  entryId <- freshNodeId
  startNewBlock entryId
  processExprs exprs
  finalizeCurrentBlock
  nodes <- gets builtNodes
  edges <- gets pendingEdges
  return (nodes, edges)

-- | Process expressions and create blocks
processExprs :: [IRExpr] -> CFGBuilder ()
processExprs [] = finalizeCurrentBlock
processExprs (irExpr:rest) = do
  let IRExpr meta exprNode = irExpr
  case exprNode of
    IRIf cond thenExpr elseExpr -> do
      -- Finalize current block with if as terminator
      modifyCurrentBlock (\b -> b { blockTerminator = Just irExpr })
      currentId <- getCurrentBlockId
      finalizeCurrentBlock

      -- Create then block
      thenId <- freshNodeId
      startNewBlock thenId
      processExprs [thenExpr]
      finalizeCurrentBlock

      -- Create else block
      elseId <- freshNodeId
      startNewBlock elseId
      processExprs [elseExpr]
      finalizeCurrentBlock

      -- Create merge block
      mergeId <- freshNodeId
      startNewBlock mergeId

      -- Record edges to be created later
      addPendingEdge currentId thenId
      addPendingEdge currentId elseId
      addPendingEdge thenId mergeId
      addPendingEdge elseId mergeId

      processExprs rest

    _ -> do
      modifyCurrentBlock (\b -> b { blockStmts = blockStmts b ++ [irExpr] })
      processExprs rest

-- Helper functions
freshNodeId :: CFGBuilder NodeId
freshNodeId = do
  id <- gets nextNodeId
  modify $ \s -> s { nextNodeId = id + 1 }
  return id

startNewBlock :: NodeId -> CFGBuilder ()
startNewBlock id =
  modify $ \s -> s { currentBlock = Just $ BasicBlock id [] Nothing }

getCurrentBlockId :: CFGBuilder NodeId
getCurrentBlockId = do
  mcurrent <- gets currentBlock
  case mcurrent of
    Just block -> return $ blockId block
    Nothing -> throwError InvalidBlock

modifyCurrentBlock :: (BasicBlock -> BasicBlock) -> CFGBuilder ()
modifyCurrentBlock f =
  modify $ \s -> s { currentBlock = f <$> currentBlock s }

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

addPendingEdge :: NodeId -> NodeId -> CFGBuilder ()
addPendingEdge from to =
  modify $ \s -> s { pendingEdges = Edge from to : pendingEdges s }

-- | Add all edges to the graph at once
addEdgesToGraph :: M.Map NodeId CFGNode -> [Edge] -> M.Map NodeId CFGNode
addEdgesToGraph nodes edges = foldr addOneEdge nodes edges
  where
    addOneEdge (Edge from to) ns =
      let ns1 = M.adjust (\n -> n { successors = S.insert to (successors n) }) from ns
          ns2 = M.adjust (\n -> n { predecessors = S.insert from (predecessors n) }) to ns1
      in ns2
