{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.AllocationOpt
  ( optimizeAllocations
  , OptimizationStats(..)
  ) where

import Data.List (sortBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad (forM_, when)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Debug.Trace

import Zap.IR.Core
import Zap.IR.Allocator
import Zap.Analysis.Allocation

-- | Statistics from optimization passes
data OptimizationStats = OptimizationStats
  { stackSavings :: Integer        -- Bytes saved in stack allocation
  , simdOptimizations :: Int       -- Number of SIMD optimizations applied
  , parameterReordering :: Int     -- Number of parameters reordered
  , inlinedParameters :: Int       -- Number of parameters inlined
  } deriving (Show, Eq)

-- | State for optimization passes
data OptState = OptState
  { currentStats :: OptimizationStats
  , parameterSizes :: M.Map T.Text Integer
  , simdParameters :: S.Set T.Text
  , accessPatterns :: M.Map T.Text Int
  } deriving (Show, Eq)

type OptM = StateT OptState (Either String)

initialState :: OptState
initialState = OptState
  { currentStats = OptimizationStats 0 0 0 0
  , parameterSizes = M.empty
  , simdParameters = S.empty
  , accessPatterns = M.empty
  }

-- | Main optimization entry point
optimizeAllocations :: IR -> Either String (IR, OptimizationStats)
optimizeAllocations ir = do
  (ir', state) <- runStateT (optimizeIR ir) initialState
  return (ir', currentStats state)

-- First let's add a helper function to decide if we can move to stack
-- A threshold for what we consider a "large" struct (in bytes)
largeObjectThreshold :: Integer
largeObjectThreshold = 256  -- Adjust this value based on target architecture

-- Calculate total size of a type
calcTypeSize :: IRType -> Integer
calcTypeSize typ = case typ of
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
    IRTypeStruct _ fields -> sum $ map (calcTypeSize . snd) fields
    _ -> 8  -- Default for other types

canMoveToStack :: IRType -> Bool
canMoveToStack typ = case typ of
    IRTypeNum _ -> True
    IRTypeBool -> True
    IRTypeVec _ -> True
    IRTypeStruct _ fields ->
        let totalSize = calcTypeSize (IRTypeStruct "" fields)
        in totalSize <= largeObjectThreshold && all (canMoveToStack . snd) fields
    _ -> False

optimizeExpr :: IRExpr -> OptM IRExpr
optimizeExpr expr = do
    traceM $ "Optimizing expression: " ++ show expr
    case expr of
        IRLetAlloc name val strat -> do
            traceM $ "Found let allocation: " ++ show name
            val' <- optimizeExpr val
            let valType = inferExprType val'
            traceM $ "Value type: " ++ show valType

            -- First determine the new strategy purely
            let newStrat = case (strat, valType) of
                    (IRAllocHeap, t) | canMoveToStack t -> IRAllocStack
                    (IRAllocDefault, IRTypeVec _) -> IRAllocStack  -- Convert default vectors to stack
                    _ -> strat

            -- Update statistics based on the transformation
            case (strat, valType, newStrat) of
                -- Count heap-to-stack conversions
                (IRAllocHeap, _, IRAllocStack) ->
                    modify $ \s -> s { currentStats = (currentStats s) {
                        stackSavings = stackSavings (currentStats s) + 1
                    } }
                -- Count SIMD optimizations
                (IRAllocDefault, IRTypeVec _, IRAllocStack) ->
                    modify $ \s -> s { currentStats = (currentStats s) {
                        simdOptimizations = simdOptimizations (currentStats s) + 1
                    } }
                _ -> return ()

            traceM $ "New allocation strategy: " ++ show newStrat
            return $ IRLetAlloc name val' newStrat

        IRVecAlloc vt components strat -> do
            traceM $ "Found vector allocation: " ++ show vt
            components' <- mapM optimizeExpr components
            -- Mark SIMD optimization
            modify $ \s -> s { currentStats = (currentStats s) {
                simdOptimizations = simdOptimizations (currentStats s) + 1
            } }
            return $ IRVecAlloc vt components' IRAllocStack

        IRBinOp op e1 e2 -> do
            e1' <- optimizeExpr e1
            e2' <- optimizeExpr e2
            return $ IRBinOp op e1' e2'

        _ -> return expr

-- Helper function to infer expression types
inferExprType :: IRExpr -> IRType
inferExprType expr = case expr of
    IRNum t _ -> IRTypeNum t
    IRString _ -> IRTypeString
    IRBool _ -> IRTypeBool
    IRVec vt _ -> IRTypeVec vt
    IRStructLit name fields ->
        -- Preserve field types when inferring struct type
        IRTypeStruct name [(fname, inferExprType fexpr) | (fname, fexpr) <- fields]
    _ -> IRTypeNum IRInt32  -- Default case

-- Update the main optimization function
optimizeIR :: IR -> OptM IR
optimizeIR (IRProgram decls exprs) = do
    -- First collect information about parameters
    mapM_ analyzeParameterUsage decls
    -- Then apply optimizations
    decls' <- mapM optimizeDecl decls
    exprs' <- mapM optimizeExpr exprs
    return $ IRProgram decls' exprs'

optimizeDecl :: IRDecl -> OptM IRDecl
optimizeDecl orig@(IRFunc name params retType body) = do
    -- Get parameter usage information
    accesses <- gets accessPatterns
    sizes <- gets parameterSizes
    simdParams <- gets simdParameters

    -- Apply optimizations based on collected data
    let optimizedParams = optimizeParameterOrder params accesses sizes simdParams

    -- Record statistics when parameters are reordered
    when (params /= optimizedParams) $
        modify $ \s -> s { currentStats = (currentStats s)
            { parameterReordering = parameterReordering (currentStats s) + 1 } }

    return $ IRFunc name optimizedParams retType body
optimizeDecl other = return other

optimizeParameterOrder :: [(T.Text, IRType)] -> M.Map T.Text Int ->
                         M.Map T.Text Integer -> S.Set T.Text ->
                         [(T.Text, IRType)]
optimizeParameterOrder params accesses sizes simdParams =
    -- Sort parameters based on:
    -- 1. SIMD vectors first (for alignment)
    -- 2. Larger sizes first
    -- 3. Most frequently accessed for tie breaks
    sortBy compareParams params
  where
    compareParams (n1, t1) (n2, t2) =
        let inSimd1 = S.member n1 simdParams
            inSimd2 = S.member n2 simdParams
            acc1 = fromMaybe 0 $ M.lookup n1 accesses
            acc2 = fromMaybe 0 $ M.lookup n2 accesses
            size1 = fromMaybe 0 $ M.lookup n1 sizes
            size2 = fromMaybe 0 $ M.lookup n2 sizes
        in case (inSimd1, inSimd2) of
             (True, False) -> LT
             (False, True) -> GT
             _ -> case compare size2 size1 of  -- Note reversed comparison for larger first
                    EQ -> compare acc2 acc1
                    other -> other

-- | Analyze how parameters are used
analyzeParameterUsage :: IRDecl -> OptM ()
analyzeParameterUsage (IRFunc _ params _ body) = do
    -- Record parameter sizes
    forM_ params $ \(name, typ) -> do
        size <- computeParameterSize typ
        modify $ \s -> s { parameterSizes = M.insert name size (parameterSizes s) }

    -- Identify SIMD-compatible parameters
    let simdParams = [name | (name, IRTypeVec _) <- params]
    modify $ \s -> s { simdParameters = S.union (S.fromList simdParams) (simdParameters s) }

    -- Analyze access patterns in function body
    analyzeAccess body
analyzeParameterUsage _ = return ()

-- | Analyze parameter access patterns in expressions
analyzeAccess :: IRExpr -> OptM ()
analyzeAccess expr = case expr of
    IRVar name ->
        modify $ \s -> s { accessPatterns = M.insertWith (+) name 1 (accessPatterns s) }
    IRBinOp _ e1 e2 -> do
        analyzeAccess e1
        analyzeAccess e2
    IRIf cond then_ else_ -> do
        analyzeAccess cond
        analyzeAccess then_
        analyzeAccess else_
    IRBlock _ exprs mResult -> do
        mapM_ analyzeAccess exprs
        mapM_ analyzeAccess mResult
    _ -> return ()

-- | Compute size needed for parameter
computeParameterSize :: IRType -> OptM Integer
computeParameterSize typ = case typ of
    IRTypeNum nt -> return $ case nt of
        IRInt32 -> 4
        IRInt64 -> 8
        IRFloat32 -> 4
        IRFloat64 -> 8
    IRTypeBool -> return 1
    IRTypeString -> return 8
    IRTypeVec vt -> return $ case vt of
        IRVec2 _ -> 8
        IRVec3 _ -> 16  -- Padded for alignment
        IRVec4 _ -> 16
    IRTypeStruct _ fields -> do
        sizes <- mapM (computeParameterSize . snd) fields
        return $ sum sizes
    IRTypeArray _ -> return 8
