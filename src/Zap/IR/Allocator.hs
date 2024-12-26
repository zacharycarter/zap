{-# LANGUAGE OverloadedStrings #-}
module Zap.IR.Allocator
  ( AllocKind(..)
  , Allocator(..)
  , AllocError(..)
  , AllocExpr(..)
  , allocateIR
  , getExprType
  ) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except
import Zap.IR.Core
import Debug.Trace

-- | Types of allocation
data AllocKind
  = HeapAlloc    -- Dynamic allocation with manual control
  | ArenaAlloc   -- Arena/pool allocation
  | StackAlloc   -- Stack allocation
  | TempAlloc    -- Temporary allocation (freed at end of scope)
  deriving (Show, Eq)

-- | Internal allocation strategy type
data InternalAllocStrat
  = SpecificAlloc AllocKind
  | DefaultAlloc
  | ScopedAlloc
  | CustomAlloc T.Text
  deriving (Show, Eq)

-- | Allocator definition
data Allocator = Allocator
  { allocKind :: AllocKind
  , allocName :: T.Text
  , allocParent :: Maybe Allocator
  , allocStats :: AllocStats
  }
  deriving (Show, Eq)

-- | Allocation statistics for optimization
data AllocStats = AllocStats
  { totalAllocs :: Int
  , totalBytes :: Integer
  , maxAlloc :: Integer
  , avgLifetime :: Double
  }
  deriving (Show, Eq)

-- | Allocation errors
data AllocError
  = NoSuitableAllocator IRType
  | InvalidAllocStrategy InternalAllocStrat IRType
  | AllocationTooLarge Integer
  | CircularAllocation T.Text
  deriving (Show, Eq)

-- | Expression with allocation information
data AllocExpr = AllocExpr
  { allocExpr :: IRExpr        -- Original expression
  , allocStrat :: InternalAllocStrat   -- Allocation strategy
  , allocSize :: Integer       -- Size in bytes
  , allocAlign :: Int          -- Alignment requirement
  }
  deriving (Show, Eq)

-- | State for allocation analysis
data AllocState = AllocState
  { currentScope :: T.Text
  , scopeAllocs :: M.Map T.Text Allocator
  , globalAllocs :: M.Map T.Text Allocator
  , allocSizes :: M.Map IRType Integer
  }

type AllocM = ExceptT AllocError (State AllocState)

-- | Convert internal allocation strategy to IR allocation strategy
convertStrat :: InternalAllocStrat -> IRAllocStrat
convertStrat strat = case strat of
  SpecificAlloc HeapAlloc -> IRAllocHeap
  SpecificAlloc StackAlloc -> IRAllocStack
  SpecificAlloc ArenaAlloc -> IRAllocArena
  SpecificAlloc TempAlloc -> IRAllocTemp
  DefaultAlloc -> IRAllocDefault
  ScopedAlloc -> IRAllocScoped
  CustomAlloc name -> IRAllocCustom name

-- | Add allocation information to IR
allocateIR :: IR -> Either AllocError IR
allocateIR ir = evalState (runExceptT $ addAllocation ir) initAllocState

-- | Initialize allocation state with default sizes
initAllocState :: AllocState
initAllocState = AllocState
  { currentScope = "global"
  , scopeAllocs = M.empty
  , globalAllocs = M.fromList
      [ ("heap", Allocator HeapAlloc "heap" Nothing defaultStats)
      , ("stack", Allocator StackAlloc "stack" Nothing defaultStats)
      ]
  , allocSizes = M.fromList
      [ (IRTypeNum IRInt32, 4)
      , (IRTypeNum IRInt64, 8)
      , (IRTypeNum IRFloat32, 4)
      , (IRTypeNum IRFloat64, 8)
      , (IRTypeBool, 1)
      ]
  }
  where
    defaultStats = AllocStats 0 0 0 0.0

-- | Add allocation to IR program
addAllocation :: IR -> AllocM IR
addAllocation (IRProgram decls exprs) = do
  allocDecls <- mapM allocateDecl decls
  allocExprs <- mapM allocateExpr exprs
  return $ IRProgram allocDecls allocExprs

-- | Add allocation to expressions
allocateExpr :: IRExpr -> AllocM IRExpr
allocateExpr ex@(IRExpr meta node) = do
    -- Helper to wrap result with metadata
    let wrap newNode = IRExpr meta { metaEffects = updateEffects ex newNode } newNode

    -- Handle each expression type
    traceM $ "Adding allocation to expression: " ++ show ex
    case node of
        IRNum _ _ ->
            return $ wrap node  -- Numeric literals don't need allocation

        IRVar name -> do
            strat <- lookupVarStrategy name
            return $ wrap $ IRVarAlloc name (convertStrat strat)

        IRLet name val -> do
            allocVal <- allocateExpr val
            strat <- determineStrategy (getExprType allocVal)
            return $ wrap $ IRLetAlloc name allocVal (convertStrat strat)

        IRVec vt components -> do
            allocComps <- mapM allocateExpr components
            strat <- determineStrategy (IRTypeVec vt)
            return $ wrap $ IRVecAlloc vt allocComps (convertStrat strat)

        IRStructLit name fields -> do
            allocFields <- mapM (\(f, e) -> do
                ae <- allocateExpr e
                return (f, ae)) fields
            strat <- determineStrategy (IRTypeStruct name [])
            return $ wrap $ IRStructLitAlloc name allocFields (convertStrat strat)

        IRBlock name exprs mResult -> do
            scope <- gets currentScope
            let scopeName = scope <> "." <> name
            modify $ \s -> s { currentScope = scopeName }

            allocExprs <- mapM allocateExpr exprs
            allocResult <- mapM allocateExpr mResult

            modify $ \s -> s { currentScope = T.takeWhile (/= '.') scopeName }

            return $ wrap $ IRBlockAlloc name allocExprs allocResult

        -- For other cases, preserve the original expression with updated effects
        _ -> return $ wrap node

-- Helper function to update effects based on allocation
updateEffects :: IRExpr -> IRExprNode -> S.Set Effect
updateEffects originalExpr node = case node of
    IRVarAlloc _ _ -> S.insert WriteEffect (metaEffects $ metadata originalExpr)
    IRLetAlloc _ _ _ -> S.insert WriteEffect (metaEffects $ metadata originalExpr)
    _ -> metaEffects $ metadata originalExpr

-- | Look up variable allocation strategy
lookupVarStrategy :: T.Text -> AllocM InternalAllocStrat
lookupVarStrategy name = do
  s <- get
  case M.lookup name (scopeAllocs s) of
    Just alloc -> return $ SpecificAlloc (allocKind alloc)
    Nothing -> case M.lookup name (globalAllocs s) of
      Just alloc -> return $ SpecificAlloc (allocKind alloc)
      Nothing -> return DefaultAlloc

-- | Determine allocation strategy for a type
determineStrategy :: IRType -> AllocM InternalAllocStrat
determineStrategy typ = case typ of
  -- Small types go on stack
  IRTypeNum _ -> return $ SpecificAlloc StackAlloc
  IRTypeBool -> return $ SpecificAlloc StackAlloc

  -- Strings are heap allocated
  IRTypeString -> return $ SpecificAlloc HeapAlloc

  -- Vectors use SIMD-aligned stack allocation
  IRTypeVec _ -> return $ SpecificAlloc StackAlloc

  -- Structs use arena allocation by default
  IRTypeStruct _ _ -> return $ SpecificAlloc ArenaAlloc

  -- Arrays are heap allocated
  IRTypeArray _ -> return $ SpecificAlloc HeapAlloc

  -- Default to heap for unknown types
  _ -> return DefaultAlloc

-- | Get type of an expression
getExprType :: IRExpr -> IRType
getExprType (IRExpr md node) = case node of
    IRNum t _ -> IRTypeNum t
    IRString _ -> IRTypeString
    IRBool _ -> IRTypeBool
    IRVec vt _ -> IRTypeVec vt
    IRVar _ -> exprType md
    IRStructLit name _ -> IRTypeStruct name []
    IRBinOp _ e1 e2 ->
        case (getExprType e1, getExprType e2) of
            (t1@(IRTypeVec _), t2@(IRTypeVec _))
                | t1 == t2 -> t1
            (IRTypeNum n1, IRTypeNum n2)
                | n1 == n2 -> IRTypeNum n1
            _ -> exprType md
    IRFieldAccess base field ->
        case getExprType base of
            IRTypeStruct _ fields ->
                fromMaybe (exprType md)
                    (lookup field [(fn, ft) | (fn, ft) <- fields])
            _ -> exprType md
    _ -> exprType md
  where
    fromMaybe def Nothing = def
    fromMaybe _ (Just x) = x

-- | Add allocation to declarations
allocateDecl :: IRDecl -> AllocM IRDecl
allocateDecl decl = case decl of
  IRFunc name params retType body -> do
    allocBody <- allocateExpr body
    return $ IRFunc name params retType allocBody
  IRStruct {} -> return decl
