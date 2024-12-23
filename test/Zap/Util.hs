{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zap.Util (mkTestExpr) where

import qualified Data.Set as S

import Zap.IR.Core
import Zap.Analysis.Allocation

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

inferInitialType :: IRExprNode -> IRType
inferInitialType = \case
    IRNum t _ -> IRTypeNum t
    IRString _ -> IRTypeString
    IRBool _ -> IRTypeBool
    IRVec vt _ -> IRTypeVec vt
    IRVar _ -> IRTypeAny  -- Conservative default
    IRBinOp _ _ _ -> IRTypeAny
    IRStructLit name _ -> IRTypeStruct name []
    _ -> IRTypeAny

inferInitialEffects :: IRExprNode -> S.Set Effect
inferInitialEffects = \case
    IRVar _ -> S.singleton ReadEffect
    IRLetAlloc _ _ _ -> S.singleton WriteEffect
    IRPrint _ -> S.singleton IOEffect
    _ -> S.singleton PureEffect
