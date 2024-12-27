{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zap.Util (mkTestExpr, mkTypedTestExpr) where

import qualified Data.Set as S

import Zap.IR.Core

-- Test utilities
mkTestExpr :: IRExprNode -> IRExpr
mkTestExpr node = IRExpr
    { metadata = IRMetadata
        { exprType = inferInitialType node
        , metaEffects = inferInitialEffects node
        , metaSourcePos = Nothing
        }
    , expr = node
    }

mkTypedTestExpr :: IRType -> IRExprNode -> IRExpr
mkTypedTestExpr typ node = IRExpr
    { metadata = IRMetadata
        { exprType = typ
        , metaEffects = inferInitialEffects node
        , metaSourcePos = Nothing
        }
    , expr = node
    }

-- Infer initial type for an IR node
inferInitialType :: IRExprNode -> IRType
inferInitialType node = case node of
    -- Simple types remain unchanged
    IRNum t _ -> IRTypeNum t
    IRString _ -> IRTypeString
    IRBool _ -> IRTypeBool
    IRVec vt _ -> IRTypeVec vt

    -- Print expressions should use type of printed expression
    -- IRPrint expr -> exprType $ metadata expr
    IRPrint _ -> IRTypeVoid

    -- Binary operations should use operand types
    IRBinOp op e1 e2 ->
        case op of
            IRLt -> IRTypeBool  -- Comparison operators
            IRGt -> IRTypeBool
            IREq -> IRTypeBool
            _ -> case (exprType $ metadata e1, exprType $ metadata e2) of
                (IRTypeNum t1, IRTypeNum t2) | t1 == t2 -> IRTypeNum t1
                (IRTypeVec v1, IRTypeVec v2) | v1 == v2 -> IRTypeVec v1
                _ -> IRTypeAny

    -- If expressions use branch types
    IRIf _ thenExpr elseExpr ->
        let thenType = exprType $ metadata thenExpr
            elseType = exprType $ metadata elseExpr
        in if thenType == elseType
           then thenType
           else IRTypeAny

    -- Result expressions should propagate child type
    IRResult rex -> exprType $ metadata rex

    -- Block expressions use result type if available
    IRBlockAlloc _ exprs mResult ->
        case mResult of
            Just result -> exprType $ metadata result
            Nothing -> case exprs of
                [] -> IRTypeAny
                _ -> exprType $ metadata $ last exprs

    -- Other cases remain unchanged
    IRVar _ -> IRTypeAny
    IRStructLit name _ -> IRTypeStruct name []
    IRLetAlloc _ laex _ -> exprType $ metadata laex
    IRVarAlloc _ _ -> IRTypeAny
    IRFieldAccess _ _ -> IRTypeAny
    IRArrayLit t _ -> IRTypeArray t
    IRIndex _ _ -> IRTypeAny
    IRVecAlloc vt _ _ -> IRTypeVec vt
    IRStructLitAlloc name _ _ -> IRTypeStruct name []
    IRCall _ _ -> IRTypeAny
    _ -> IRTypeAny

-- | Infer initial effects for an IR node
inferInitialEffects :: IRExprNode -> S.Set Effect
inferInitialEffects = \case
    -- Pure operations
    IRNum _ _ -> S.singleton PureEffect
    IRString _ -> S.singleton PureEffect
    IRBool _ -> S.singleton PureEffect
    IRVec _ _ -> S.singleton PureEffect
    IRStructLit _ _ -> S.singleton PureEffect
    IRArrayLit _ _ -> S.singleton PureEffect

    -- Memory operations
    IRLetAlloc _ _ _ -> S.fromList [WriteEffect, PureEffect]
    IRVarAlloc _ _ -> S.fromList [WriteEffect, PureEffect]
    IRVecAlloc _ _ _ -> S.fromList [WriteEffect, PureEffect]
    IRStructLitAlloc _ _ _ -> S.fromList [WriteEffect, PureEffect]

    -- Variable access
    IRVar _ -> S.singleton ReadEffect
    IRFieldAccess _ _ -> S.singleton ReadEffect
    IRIndex _ _ -> S.singleton ReadEffect

    -- IO operations
    IRPrint _ -> S.singleton IOEffect

    -- Control flow
    IRBlockAlloc _ _ _ -> S.empty  -- Effects come from contained expressions
    IRBreak _ -> S.empty
    IRResult _ -> S.singleton PureEffect
    IRIf _ _ _ -> S.singleton PureEffect

    -- Function calls can have any effect
    IRCall _ _ -> S.empty

    -- Binary operations are pure
    IRBinOp _ _ _ -> S.singleton PureEffect

    _ -> S.empty

-- inferInitialType :: IRExprNode -> IRType
-- inferInitialType = \case
--     IRNum t _ -> IRTypeNum t
--     IRString _ -> IRTypeString
--     IRBool _ -> IRTypeBool
--     IRVec vt _ -> IRTypeVec vt
--     IRVar _ -> IRTypeAny  -- Conservative default
--     IRBinOp _ _ _ -> IRTypeAny
--     IRStructLit name _ -> IRTypeStruct name []
--     _ -> IRTypeAny

-- inferInitialEffects :: IRExprNode -> S.Set Effect
-- inferInitialEffects = \case
--     IRVar _ -> S.singleton ReadEffect
--     IRLetAlloc _ _ _ -> S.singleton WriteEffect
--     IRPrint _ -> S.singleton IOEffect
--     _ -> S.singleton PureEffect
