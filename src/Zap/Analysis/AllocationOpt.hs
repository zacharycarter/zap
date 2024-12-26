{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.AllocationOpt
  ( optimizeAllocations
  , isVectorType
  , OptimizationStats(..)
  ) where

import Data.List (sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad (foldM, forM_, when)
import Control.Monad.State
import Debug.Trace

import Zap.IR.Core

data OptimizationStats = OptimizationStats
  { stackSavings :: Integer
  , simdOptimizations :: Int
  , parameterReordering :: Int
  , inlinedParameters :: Int
  } deriving (Show, Eq)

data StructInfo = StructInfo
  { structFields :: [(T.Text, IRType)]
  , structSize :: Integer
  } deriving (Show, Eq)
 

data OptState = OptState
  { currentStats :: OptimizationStats
  , parameterSizes :: M.Map T.Text Integer
  , simdParameters :: S.Set T.Text
  , accessPatterns :: M.Map T.Text Int
  , structReg :: M.Map T.Text StructInfo
  } deriving (Show, Eq)

type OptM = StateT OptState (Either String)

largeObjectThreshold :: Integer
largeObjectThreshold = 256

initialState :: OptState
initialState = OptState
  { currentStats = OptimizationStats 0 0 0 0
  , parameterSizes = M.empty
  , simdParameters = S.empty
  , accessPatterns = M.empty
  , structReg = M.empty
  }

calculateStructSize :: M.Map T.Text StructInfo -> IRType -> Integer
calculateStructSize reg typ = case typ of
    IRTypeStruct name fields ->
        case M.lookup name reg of
            Just info -> structSize info
            Nothing ->
                -- If not in registry, calculate directly from fields
                sum $ map (calculateStructSize reg . snd) fields
    _ -> calculateSize typ

calculateSize :: IRType -> Integer
calculateSize typ = case typ of
    IRTypeNum nt -> case nt of
        IRInt32 -> 4
        IRInt64 -> 8
        IRFloat32 -> 4
        IRFloat64 -> 8
    IRTypeBool -> 1
    IRTypeVec vt -> case vt of
        IRVec2 _ -> 8
        IRVec3 _ -> 12
        IRVec4 _ -> 16
    _ -> 0

optimizeAllocations :: IR -> Either String (IR, OptimizationStats)
optimizeAllocations ir = do
  (ir', st) <- runStateT (optimizeIR ir) initialState
  return (ir', currentStats st)

optimizeIR :: IR -> OptM IR
optimizeIR (IRProgram decls exprs) = do
    traceM "Starting IR optimization..."
    registry <- buildStructRegistry decls
    modify $ \s -> s { structReg = registry }
    decls' <- mapM optimizeDecl decls
    exprs' <- mapM optimizeExpr exprs
    stats <- gets currentStats
    traceM $ "Optimization complete. Stats: " ++ show stats
    return $ IRProgram decls' exprs'

buildStructRegistry :: [IRDecl] -> OptM (M.Map T.Text StructInfo)
buildStructRegistry decls = do
    let structs = [(name, fields) | IRStruct name fields <- decls]
    traceM $ "Building struct registry with structs: " ++ show structs
    registry <- foldM addStructInfo M.empty structs
    traceM $ "Final registry contents: " ++ show registry
    return registry
  where
    addStructInfo reg (name, fields) = do
        let size = sum $ map (calculateStructSize reg . snd) fields
        traceM $ "Adding struct " ++ show name ++ " with size " ++ show size
        return $ M.insert name (StructInfo fields size) reg

optimizeExpr :: IRExpr -> OptM IRExpr
optimizeExpr _irExpr@(IRExpr meta exprNode) = case exprNode of
    IRLetAlloc name val strat -> do
        traceM $ "Optimizing allocation for " ++ show name
        val' <- optimizeExpr val
        case val' of
            IRExpr _ (IRStructLit _ fields) -> do
                let fieldTypes = map (exprType . metadata . snd) fields
                reg <- gets structReg
                let ss = sum $ map (calculateStructSize reg) fieldTypes
                traceM $ "Calculated struct size: " ++ show ss
                let newStrat = if ss > largeObjectThreshold
                             then IRAllocHeap
                             else IRAllocStack

                when (strat == IRAllocHeap && newStrat == IRAllocStack) $ do
                    modify $ \s -> s { currentStats = (currentStats s) {
                        stackSavings = stackSavings (currentStats s) + ss
                    }}

                return $ IRExpr meta { exprType = exprType (metadata val') }
                       (IRLetAlloc name val' newStrat)

            IRExpr _ (IRVec vt components) -> do
              let valType = exprType $ metadata val'
              traceM $ "Processing vector allocation of type " ++ show vt
              components' <- mapM optimizeExpr components
              let (newStrat, isSIMD) = alignVectorAllocation vt strat

              -- Track SIMD optimization when moving to stack allocation
              when (strat /= newStrat && isSIMD) $ do
                modify $ \s -> s { currentStats = (currentStats s) {
                    simdOptimizations = simdOptimizations (currentStats s) + 1
                }}
                traceM $ "Incremented SIMD optimizations counter for vector type " ++ show vt

              let res = IRExpr meta { exprType = valType } (IRLetAlloc name (IRExpr meta { exprType = valType } (IRVecAlloc vt components' newStrat)) newStrat)
              traceM $ "Returning: " ++ show res
              return $ res

            _ -> do
              let valType = exprType $ metadata val'
              let size = calculateSize valType
              traceM $ "Calculated size: " ++ show size
              let newStrat = if size > largeObjectThreshold
                    then IRAllocHeap
                    else IRAllocStack

              when (strat == IRAllocHeap && newStrat == IRAllocStack) $ do
                  modify $ \s -> s { currentStats = (currentStats s) {
                      stackSavings = stackSavings (currentStats s) + size
                  }}

              return $ IRExpr meta { exprType = valType } (IRLetAlloc name val' newStrat)

    IRVecAlloc vt components strat -> do
        traceM $ "Processing vector allocation of type " ++ show vt
        components' <- mapM optimizeExpr components
        let (newStrat, isSIMD) = alignVectorAllocation vt strat

        -- Track SIMD optimization when moving to stack allocation
        when (strat /= newStrat && isSIMD) $ do
            modify $ \s -> s { currentStats = (currentStats s) {
                simdOptimizations = simdOptimizations (currentStats s) + 1
            }}
            traceM $ "Incremented SIMD optimizations counter for vector type " ++ show vt

        return $ IRExpr meta (IRVecAlloc vt components' newStrat)

    _ -> do
      traceM $ "Skipping optimization for expr node: " ++ show exprNode
      return $ IRExpr meta exprNode

alignVectorAllocation :: IRVecType -> IRAllocStrat -> (IRAllocStrat, Bool)
alignVectorAllocation vt _ = case vt of
    IRVec4 _ -> (IRAllocStack, True)   -- Always SIMD capable
    IRVec2 _ -> (IRAllocStack, True)   -- Can use half SSE register
    IRVec3 _ -> (IRAllocStack, False)  -- Not SIMD optimal due to padding

optimizeDecl :: IRDecl -> OptM IRDecl
optimizeDecl _decl@(IRFunc name params retType body) = do
    traceM $ "Optimizing function: " ++ show name
    -- Record parameter sizes and SIMD info
    forM_ params $ \(pname, ptype) -> do
        traceM $ "Optimizing parameter " ++ show pname ++ " with type " ++ show ptype ++ " for function: " ++ show name
        let psize = calculateSize ptype
        traceM $ "Parameter " ++ show pname ++ " with type " ++ show ptype ++ " has size: " ++ show psize
        modify $ \s -> s {
            parameterSizes = M.insert pname (calculateSize ptype) (parameterSizes s),
            simdParameters = if isVectorType ptype
                           then S.insert pname (simdParameters s)
                           else simdParameters s
        }

    let orderedParams = reorderParameters params
    when (orderedParams /= params) $ do
        modify $ \s -> s { currentStats = (currentStats s) {
            parameterReordering = parameterReordering (currentStats s) + 1
        }}
        traceM $ "Parameters reordered for better packing: " ++ show params

    body' <- optimizeExpr body
    let result = IRFunc name orderedParams retType body'
    traceM $ "Result: " ++ show result
    return $ result
optimizeDecl other = return other

-- optimizeDecl :: IRDecl -> OptM IRDecl
-- optimizeDecl decl@(IRFunc name params retType body) = do
--     traceM $ "Optimizing function: " ++ show name

--     forM_ params $ \(pname, ptype) -> do
--         reg <- gets structReg
--         modify $ \s -> s {
--             parameterSizes = M.insert pname (calculateSize reg ptype) (parameterSizes s),
--             simdParameters = if isVectorType ptype
--                            then S.insert pname (simdParameters s)
--                            else simdParameters s
--         }

--     let orderedParams = reorderParameters params
--     when (orderedParams /= params) $ do
--         traceM "Parameters reordered for better packing"
--         modify $ \s -> s { currentStats = (currentStats s) {
--             parameterReordering = parameterReordering (currentStats s) + 1
--         }}

--     body' <- optimizeExpr body
--     return $ IRFunc name orderedParams retType body'
-- optimizeDecl other = return other

reorderParameters :: [(T.Text, IRType)] -> [(T.Text, IRType)]
reorderParameters = sortBy (\(_, t1) (_, t2) -> compare (parameterPriority t2) (parameterPriority t1))

parameterPriority :: IRType -> Integer
parameterPriority typ = case typ of
    IRTypeVec _ -> 1000   -- SIMD vectors first
    t -> calcBasePriority t
  where
    calcBasePriority t = case t of
        IRTypeNum nt -> case nt of
            IRInt64 -> 8
            IRFloat64 -> 8
            _ -> 4
        IRTypeBool -> 1
        _ -> 0

isVectorType :: IRType -> Bool
isVectorType (IRTypeVec _) = True
isVectorType _ = False
