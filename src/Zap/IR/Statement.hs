{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zap.IR.Statement
  ( IRBasicBlock(..)
  , IRError(..)
  , IRFunction(..)
  , IRSeq(..)
  , IRStmt(..)
  , validateBasicBlock
  , validateFunction
  , validateSeq
  ) where

import Data.Array ((!))
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))
import Control.Monad (foldM, foldM_, forM_, unless, when)
import Control.Monad.Except
import qualified Data.Graph as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace

import Zap.IR.Core
  ( IRExpr(..)
  , IRExprNode(..)
  , IRType(..)
  , exprType
  , metadata
  )

type Label = T.Text

data IRStmt
  = IRLabel Label
  | IRJump Label
  | IRCondJump
      { condJumpCond :: IRExpr
      , condJumpThen :: Label
      , condJumpElse :: Label
      }
  | IRExprStmt IRExpr
  | IRAssign T.Text IRExpr
  | IRReturn (Maybe IRExpr)
  | IRVarDecl          -- New: Variable declaration
      { varDeclName :: T.Text
      , varDeclType :: IRType
      , varDeclInit :: Maybe IRExpr
      }
  | IRWhile
      { whileCond :: IRExpr
      , whileBody :: [IRStmt]
      }
  deriving (Show, Eq)

data IRFunction = IRFunction
  { funcName :: T.Text
  , funcParams :: [(T.Text, IRType)]
  , funcRetType :: IRType
  , funcBody :: IRSeq
  } deriving (Show, Eq)

data IRBasicBlock = IRBasicBlock
  { blockName :: T.Text
  , blockStmts :: [IRStmt]
  } deriving (Show, Eq)

data IRSeq = IRSeq
  { seqBlocks :: [IRBasicBlock]
  } deriving (Show, Eq)

data IRError
  -- Function validation errors
  = TypeMismatch IRType IRType
  | MissingReturn
  | InvalidReturn
  -- Block validation errors
  | EmptyBasicBlock T.Text
  | InvalidTerminator T.Text IRStmt
  -- Seq validation errors
  | DisconnectedFlow T.Text
  | UnreachableBlock T.Text
  | DuplicateLabel T.Text
  -- Declaration validation errors
  | UndeclaredVariable T.Text
  -- Loop validation errors
  | UnmodifiedLoopVar T.Text
  deriving (Show, Eq)

-- Scope tracking for validation
data Scope = Scope
  { scopeVars :: M.Map T.Text IRType
  , parentScope :: Maybe Scope
  , modifiedVars :: S.Set T.Text
  } deriving (Show, Eq)

data ValidationMode = Strict | Permissive
  deriving (Show, Eq)

validateBasicBlockStmt :: Scope -> IRStmt -> Either IRError Scope
validateBasicBlockStmt scope = \case
  IRAssign var expr -> do
    let exprType' = exprType $ metadata expr
    case lookupVar var scope of
      Just varType ->
        if exprType' /= varType
          then Left $ TypeMismatch varType exprType'
          else return scope
      -- Implicitly add undeclared variables in basic block validation
      Nothing -> return $ scope { scopeVars = M.insert var exprType' (scopeVars scope) }

  stmt -> validateStmtWithScope scope stmt

validateBasicBlock :: IRBasicBlock -> Either IRError ()
validateBasicBlock (IRBasicBlock name stmts) = do
  when (null stmts) $ Left $ EmptyBasicBlock name

  -- Validate each statement with scope tracking
  let initialScope = Scope M.empty Nothing S.empty
  _ <- foldM validateBasicBlockStmt initialScope stmts

  -- Validate terminator
  case lastMaybe stmts of
    Nothing -> Left $ EmptyBasicBlock name
    Just stmt -> case stmt of
      IRJump _ -> Right ()
      IRCondJump {} -> Right ()
      IRReturn _ -> Right ()
      IRWhile {} -> Right ()
      other -> Left $ InvalidTerminator name other

-- Helper for safe last element
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

-- | Validate a sequence of basic blocks
validateSeq :: IRSeq -> Either IRError ()
validateSeq (IRSeq blocks) = do
  -- First validate individual blocks
  mapM_ (either (error . show) return . validateBasicBlock) blocks

  -- Build sets of defined labels and jump targets
  let blockLabels = S.fromList $ map blockName blocks
      jumpTargets = S.fromList $ concatMap getJumpTargets blocks

  -- Check for jumps to non-existent blocks
  case S.toList $ S.difference jumpTargets blockLabels of
    (missing:_) -> Left $ DisconnectedFlow missing
    [] -> do
      -- Check for unreachable blocks
      case findUnreachableBlocks blocks of
        (unreachable:_) -> Left $ UnreachableBlock (blockName unreachable)
        [] -> Right ()

-- | Get all jump targets from a block
getJumpTargets :: IRBasicBlock -> [T.Text]
getJumpTargets block = mapMaybe getTarget (blockStmts block)
  where
    getTarget :: IRStmt -> Maybe T.Text
    getTarget (IRJump target) = Just target
    getTarget (IRCondJump _ thn els) = Just thn <> Just els
    getTarget _ = Nothing

-- | Build a directed graph representing control flow
buildFlowGraph :: [IRBasicBlock] -> (G.Graph, G.Vertex -> ((), T.Text, [T.Text]), T.Text -> Maybe G.Vertex)
buildFlowGraph blocks = G.graphFromEdges edges
  where
    edges = [((), name, getJumpTargets block) | block@(IRBasicBlock name _) <- blocks]

-- | Find blocks that can't be reached from entry
findUnreachableBlocks :: [IRBasicBlock] -> [IRBasicBlock]
findUnreachableBlocks blocks =
  case blocks of
    [] -> []
    (entry:_) -> -- Assume first block is entry
      let (graph, nodeFromVertex, vertexFromKey) = buildFlowGraph blocks
          entryVertex = case vertexFromKey (blockName entry) of
                         Just v -> v
                         Nothing -> error "Entry block not found in graph"
          reachable = S.fromList $ G.reachable graph entryVertex
          allVertices = S.fromList [0..length blocks - 1]
          unreachableVertices = S.difference allVertices reachable
      in [block | v <- S.toList unreachableVertices,
                 let (_, name, _) = nodeFromVertex v,
                 block <- blocks,
                 blockName block == name]

validateFunction :: IRFunction -> Either IRError ()
validateFunction func@(IRFunction funcName funcParams funcRetType funcBody) = do
  -- Create initial scope with parameters
  let initialScope = Scope
        { scopeVars = M.fromList funcParams
        , parentScope = Nothing
        , modifiedVars = S.empty
        }

  -- Validate sequence with scope
  validateSeqWithScope initialScope funcBody

  -- Continue with existing return validation
  validateFunctionReturns func

validateSeqWithScope :: Scope -> IRSeq -> Either IRError ()
validateSeqWithScope scope (IRSeq blocks) = do
  -- Validate blocks while tracking scope
  foldM_ validateBlockWithScope scope blocks
  where
    validateBlockWithScope :: Scope -> IRBasicBlock -> Either IRError Scope
    validateBlockWithScope scope block = do
      let stmts = blockStmts block
      foldM validateStmtWithScope scope stmts



validateStmtWithScope :: Scope -> IRStmt -> Either IRError Scope
validateStmtWithScope scope = do
  traceM $ "\n=== Validating Statement With Scope ==="
  \case
    IRWhile cond body -> do
      -- traceM $ "\n=== Validating While Statement ==="
      -- traceM $ "Current scope: " ++ show scope
      -- traceM $ "Condition: " ++ show cond
      -- traceM $ "Body length: " ++ show (length body)

      -- -- First check condition type
      -- let condType = exprType $ metadata cond
      -- traceM $ "Condition type: " ++ show condType
      -- unless (condType == IRTypeBool) $
      --   throwError $ TypeMismatch IRTypeBool condType

      -- -- Then validate body to build up scope
      -- traceM "Validating loop body with scope:"
      -- bodyScope <- foldM validateStmtWithScope scope body
      -- traceM $ "Loop body scope: " ++ show bodyScope

      -- -- Now validate condition with full scope
      -- validateExprScope bodyScope cond

      -- return bodyScope
      traceM $ "\n=== Validating While Statement ==="
      traceM $ "Current scope: " ++ show scope
      traceM $ "Condition: " ++ show cond
      traceM $ "Body length: " ++ show (length body)

      -- First check condition type
      let condType = exprType $ metadata cond
      traceM $ "Condition type: " ++ show condType
      unless (condType == IRTypeBool) $
          throwError $ TypeMismatch IRTypeBool condType

      -- Then collect variables referenced in condition
      let condVars = collectVarRefs cond
      traceM $ "Condition vars: " ++ show condVars

      -- Validate body with current scope
      bodyScope <- foldM validateStmtWithScope scope body

      -- Now validate condition with full scope
      validateExprScope bodyScope cond

      -- Check that condition variables are modified
      forM_ condVars $ \var -> do
        unless (var `S.member` modifiedVars bodyScope) $
          throwError $ UnmodifiedLoopVar var

      return bodyScope

    IRVarDecl name typ mInit -> do
      traceM $ "\n=== Validating Variable Declaration ==="
      traceM $ "Declaring: " ++ show name ++ " : " ++ show typ
      traceM $ "Current scope: " ++ show scope
      -- Check initialization type matches declaration
      forM_ mInit $ \init -> do
        let initType = exprType $ metadata init
        when (initType /= typ) $
          Left $ TypeMismatch typ initType

      -- Add variable to scope
      return $ scope { scopeVars = M.insert name typ (scopeVars scope) }

    IRAssign var expr -> do
      traceM $ "\n=== Validating Assignment Statement ==="
      let exprType' = exprType $ metadata expr
      -- If variable is declared, check type matches
      case lookupVar var scope of
        Just varType ->
          if exprType' /= varType
            then Left $ TypeMismatch varType exprType'
            else return $ scope { modifiedVars = S.insert var (modifiedVars scope) }
        -- Reject undeclared variables in function validation
        Nothing -> Left $ UndeclaredVariable var

    _ -> do
      traceM $ "\n=== Validating Unrecognized Statement ==="
      traceM $ "Current scope: " ++ show scope
      return scope

-- Helper to collect variable references from expressions
collectVarRefs :: IRExpr -> S.Set T.Text
collectVarRefs (IRExpr _ node) = case node of
  IRVar name -> S.singleton name
  IRBinOp _ e1 e2 -> collectVarRefs e1 `S.union` collectVarRefs e2
  _ -> S.empty

-- Helper for expression scope validation
validateExprScope :: Scope -> IRExpr -> Either IRError IRExpr
validateExprScope scope expr@(IRExpr meta node) = case node of
    IRVar name -> do
        traceM $ "Checking variable " ++ show name ++ " in scope"
        case M.lookup name (scopeVars scope) of
            Just varType -> do
                -- Return expr with correct type from scope
                return $ IRExpr meta { exprType = varType } node
            Nothing -> throwError $ UndeclaredVariable name

    IRBinOp op e1 e2 -> do
        e1' <- validateExprScope scope e1
        e2' <- validateExprScope scope e2
        -- Return expr with validated subexpressions
        return $ IRExpr meta (IRBinOp op e1' e2')

    _ -> return expr

-- Helper to look up variable in scope chain
lookupVar :: T.Text -> Scope -> Maybe IRType
lookupVar name scope =
  M.lookup name (scopeVars scope) <|>
  (parentScope scope >>= lookupVar name)

-- Break out return validation
validateFunctionReturns :: IRFunction -> Either IRError ()
validateFunctionReturns (IRFunction funcName funcParams funcRetType funcBody) = do
  -- Previous return validation logic
  forM_ (findReturns funcBody) $ \retExpr ->
    case (funcRetType, retExpr) of
      (IRTypeVoid, Nothing) -> return ()
      (IRTypeVoid, Just _) -> Left InvalidReturn
      (expected, Nothing) -> Left MissingReturn
      (expected, Just expr) -> do
        let actual = exprType $ metadata expr
        when (actual /= expected) $
          Left $ TypeMismatch expected actual

  when (funcRetType /= IRTypeVoid && null (findReturns funcBody)) $
    Left MissingReturn

findReturns :: IRSeq -> [Maybe IRExpr]
findReturns (IRSeq blocks) =
  [ ret | block <- blocks
        , IRReturn ret <- blockStmts block
        ]
