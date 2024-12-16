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

import Zap.IR.Core
import Zap.IR.Allocator
import Zap.Analysis.Allocation

-- | Statistics from optimization passes
data OptimizationStats = OptimizationStats
  { stackSavings :: Integer        -- Bytes saved in stack allocation
  , simdOptimizations :: Int       -- Number of SIMD optimizations applied
  , parameterReordering :: Int     -- Number of parameters reordered
  , inlinedParameters :: Int       -- Number of parameters inlined
  }

-- | State for optimization passes
data OptState = OptState
  { currentStats :: OptimizationStats
  , parameterSizes :: M.Map T.Text Integer
  , simdParameters :: S.Set T.Text
  , accessPatterns :: M.Map T.Text Int
  }

type OptM = StateT OptState (Either String)

-- | Main optimization entry point
optimizeAllocations :: IR -> Either String (IR, OptimizationStats)
optimizeAllocations ir = do
  (ir', state) <- runStateT (optimizeIR ir) initialState
  return (ir', currentStats state)
  where
    initialState = OptState
      { currentStats = OptimizationStats 0 0 0 0
      , parameterSizes = M.empty
      , simdParameters = S.empty
      , accessPatterns = M.empty
      }

-- | Optimize IR program
optimizeIR :: IR -> OptM IR
optimizeIR (IRProgram decls exprs) = do
  -- First collect information about parameters
  mapM_ analyzeParameterUsage decls
  -- Then apply optimizations
  decls' <- mapM optimizeDecl decls
  return $ IRProgram decls' exprs

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

-- | Optimize function declarations
optimizeDecl :: IRDecl -> OptM IRDecl
optimizeDecl orig@(IRFunc name params retType body) = do
  -- Get parameter usage information
  accesses <- gets accessPatterns
  sizes <- gets parameterSizes
  simdParams <- gets simdParameters

  -- Apply optimizations based on collected data
  let optimizedParams = optimizeParameterOrder params accesses sizes simdParams

  -- Record statistics
  when (params /= optimizedParams) $
    modify $ \s -> s { currentStats = (currentStats s)
      { parameterReordering = parameterReordering (currentStats s) + 1 } }

  return $ IRFunc name optimizedParams retType body
optimizeDecl other = return other

-- | Optimize parameter ordering for better performance
optimizeParameterOrder :: [(T.Text, IRType)] -> M.Map T.Text Int ->
                         M.Map T.Text Integer -> S.Set T.Text ->
                         [(T.Text, IRType)]
optimizeParameterOrder params accesses sizes simdParams =
  -- Sort parameters based on:
  -- 1. SIMD parameters first (for alignment)
  -- 2. Most frequently accessed
  -- 3. Smaller sizes first (reduce stack frame size)
  sortBy compareParams params
  where
    compareParams (n1, _) (n2, _) =
      let inSimd1 = S.member n1 simdParams
          inSimd2 = S.member n2 simdParams
          acc1 = fromMaybe 0 $ M.lookup n1 accesses
          acc2 = fromMaybe 0 $ M.lookup n2 accesses
          size1 = fromMaybe 0 $ M.lookup n1 sizes
          size2 = fromMaybe 0 $ M.lookup n2 sizes
      in case (inSimd1, inSimd2) of
           (True, False) -> LT
           (False, True) -> GT
           _ -> case compare acc2 acc1 of
                  EQ -> compare size1 size2
                  other -> other
