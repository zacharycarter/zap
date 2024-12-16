{-# LANGUAGE OverloadedStrings #-}
module Zap.IR.Core
  ( IR(..)
  , IRExpr(..)
  , IROp(..)
  , IRType(..)
  , IRDecl(..)
  , IRNumType(..)
  , IRVecType(..)
  , IRAllocStrat(..)
  ) where

import qualified Data.Text as T

data IRNumType
  = IRInt32
  | IRInt64
  | IRFloat32
  | IRFloat64
  deriving (Eq, Show, Ord)  -- Added Ord

data IRVecType
  = IRVec2 IRNumType
  | IRVec3 IRNumType
  | IRVec4 IRNumType
  deriving (Eq, Show, Ord)  -- Added Ord

data IRType
  = IRTypeNum IRNumType
  | IRTypeVec IRVecType
  | IRTypeString
  | IRTypeBool
  | IRTypeStruct T.Text [(T.Text, IRType)]
  | IRTypeArray IRType
  deriving (Eq, Show)

-- Custom Ord instance for IRType
instance Ord IRType where
  compare (IRTypeNum n1) (IRTypeNum n2) = compare n1 n2
  compare (IRTypeVec v1) (IRTypeVec v2) = compare v1 v2
  compare IRTypeString IRTypeString = EQ
  compare IRTypeBool IRTypeBool = EQ
  compare (IRTypeStruct name1 fields1) (IRTypeStruct name2 fields2) =
    case compare name1 name2 of
      EQ -> compare (map snd fields1) (map snd fields2)
      x -> x
  compare (IRTypeArray t1) (IRTypeArray t2) = compare t1 t2

  -- Define total ordering between different constructors
  compare (IRTypeNum _) _ = LT
  compare _ (IRTypeNum _) = GT
  compare (IRTypeVec _) _ = LT
  compare _ (IRTypeVec _) = GT
  compare IRTypeString _ = LT
  compare _ IRTypeString = GT
  compare IRTypeBool _ = LT
  compare _ IRTypeBool = GT
  compare (IRTypeStruct _ _) _ = LT
  compare _ (IRTypeStruct _ _) = GT

data IROp
  = IRAdd
  | IRSub
  | IRMul
  | IRDiv
  | IRDot
  | IRCross
  deriving (Show, Eq, Ord)  -- Added Ord

data IRExpr
  = IRString T.Text
  | IRNum IRNumType T.Text
  | IRVar T.Text
  | IRLet T.Text IRExpr
  | IRPrint IRExpr
  | IRBinOp IROp IRExpr IRExpr
  | IRBool Bool
  | IRIf IRExpr IRExpr IRExpr
  | IRCall T.Text [IRExpr]
  | IRBlock T.Text [IRExpr] (Maybe IRExpr)
  | IRBreak T.Text
  | IRResult IRExpr
  | IRVec IRVecType [IRExpr]
  | IRStructLit T.Text [(T.Text, IRExpr)]
  | IRFieldAccess IRExpr T.Text
  | IRArrayLit IRType [IRExpr]
  | IRIndex IRExpr IRExpr
  | IRVarAlloc T.Text IRAllocStrat
  | IRLetAlloc T.Text IRExpr IRAllocStrat
  | IRVecAlloc IRVecType [IRExpr] IRAllocStrat
  | IRStructLitAlloc T.Text [(T.Text, IRExpr)] IRAllocStrat
  | IRBlockAlloc T.Text [IRExpr] (Maybe IRExpr)
  deriving (Show, Eq)

data IRDecl
  = IRFunc T.Text [(T.Text, IRType)] IRType IRExpr
  | IRStruct T.Text [(T.Text, IRType)]
  deriving (Show, Eq)

data IR = IRProgram [IRDecl] [IRExpr]
  deriving (Show, Eq)

data IRAllocStrat
  = IRAllocHeap      -- Dynamic allocation
  | IRAllocStack     -- Stack allocation
  | IRAllocArena     -- Arena/pool allocation
  | IRAllocTemp      -- Temporary allocation
  | IRAllocDefault   -- Use default strategy
  | IRAllocScoped    -- Use enclosing scope's allocator
  | IRAllocCustom T.Text  -- Use named custom allocator
  deriving (Show, Eq, Ord)  -- Added Ord
