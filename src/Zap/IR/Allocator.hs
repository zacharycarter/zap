{-# LANGUAGE OverloadedStrings #-}
module Zap.IR.Allocator
  ( AllocKind(..)
  , Allocator(..)
  , AllocError(..)
  , AllocExpr(..)
  , allocateIR
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Except
import Zap.IR.Core

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
allocateExpr expr = case expr of
  IRNum t val -> return expr  -- Numeric literals don't need allocation

  IRVar name -> do
    -- Look up variable's allocation strategy
    strat <- lookupVarStrategy name
    return $ IRVarAlloc name (convertStrat strat)

  IRLet name val -> do
    -- Analyze value allocation
    allocVal <- allocateExpr val
    -- Determine allocation strategy for binding
    strat <- determineStrategy (getExprType allocVal)
    return $ IRLetAlloc name allocVal (convertStrat strat)

  IRVec vt components -> do
    -- Analyze component allocations
    allocComps <- mapM allocateExpr components
    -- Determine vector allocation strategy
    strat <- determineStrategy (IRTypeVec vt)
    return $ IRVecAlloc vt allocComps (convertStrat strat)

  IRStructLit name fields -> do
    -- Analyze field allocations
    allocFields <- mapM (\(f, e) -> do
      ae <- allocateExpr e
      return (f, ae)) fields
    -- Determine struct allocation strategy
    strat <- determineStrategy (IRTypeStruct name [])
    return $ IRStructLitAlloc name allocFields (convertStrat strat)

  IRBlock name exprs mResult -> do
    -- Create new scope using string concatenation
    scope <- gets currentScope
    let scopeName = scope <> "." <> name
    modify $ \s -> s { currentScope = scopeName }

    -- Analyze expressions with block scope
    allocExprs <- mapM allocateExpr exprs
    allocResult <- mapM allocateExpr mResult

    -- Restore previous scope
    modify $ \s -> s { currentScope = T.takeWhile (/= '.') scopeName }

    return $ IRBlockAlloc name allocExprs allocResult

  -- Handle other cases...
  _ -> return expr

-- | Look up variable allocation strategy
lookupVarStrategy :: T.Text -> AllocM InternalAllocStrat
lookupVarStrategy name = do
  state <- get
  case M.lookup name (scopeAllocs state) of
    Just alloc -> return $ SpecificAlloc (allocKind alloc)
    Nothing -> case M.lookup name (globalAllocs state) of
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
getExprType expr = case expr of
  IRNum t _ -> IRTypeNum t
  IRString _ -> IRTypeString
  IRBool _ -> IRTypeBool
  IRVec vt _ -> IRTypeVec vt
  IRStructLit name _ -> IRTypeStruct name []
  _ -> error "Cannot determine type of expression"

-- | Add allocation to declarations
allocateDecl :: IRDecl -> AllocM IRDecl
allocateDecl decl = case decl of
  IRFunc name params retType body -> do
    allocBody <- allocateExpr body
    return $ IRFunc name params retType allocBody
  IRStruct {} -> return decl
