{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Zap.Compiler.OptConfig
  ( OptimizationConfig(..)
  , withOptimizationConfig
  , OptimizationLevel(..)
  ) where

import Control.Monad (foldM)
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Zap.IR.Core (IR)

-- | Optimization configuration
data OptimizationConfig = OptimizationConfig
  { optLevel :: Int
  , debugInfo :: Bool
  }

data OptimizationPassConfig = OptimizationPassConfig
  { passName :: String
  , configDebugInfo :: Bool  -- Using the renamed field
  }

-- | Optimization levels
data OptimizationLevel
  = O0  -- No optimization
  | O1  -- Basic optimizations
  | O2  -- Aggressive optimizations
  | O3  -- Maximum optimizations
  deriving (Show, Eq, Ord)

-- | Run an action with optimization configuration
withOptimizationConfig :: MonadReader OptimizationConfig m
                      => m a
                      -> OptimizationConfig
                      -> m a
withOptimizationConfig action config =
    local (const config) action

-- | Get optimization passes for a given level
getOptimizationPasses :: OptimizationLevel -> [OptimizationPass]
getOptimizationPasses O0 = []
getOptimizationPasses O1 =
  [ BasicAllocation
  , ConstantFolding
  , DeadCodeElimination
  ]
getOptimizationPasses O2 = getOptimizationPasses O1 ++
  [ CommonSubexpression
  , LoopOptimization
  , VectorAlignment
  ]
getOptimizationPasses O3 = getOptimizationPasses O2 ++
  [ AggressiveInlining
  , AutoVectorization
  , GlobalValueNumbering
  ]

-- | Available optimization passes
data OptimizationPass
  = BasicAllocation
  | ConstantFolding
  | DeadCodeElimination
  | CommonSubexpression
  | LoopOptimization
  | VectorAlignment
  | AggressiveInlining
  | AutoVectorization
  | GlobalValueNumbering
  deriving (Show, Eq)

-- | Run optimization passes
runOptimizationPasses :: OptimizationConfig -> IR -> IO IR
runOptimizationPasses config ir = do
    let passes = getOptimizationPasses $ optimizationLevelFromInt $ optLevel config
    foldM (runOptimizationPass config) ir passes

-- | Run a single optimization pass
runOptimizationPass :: OptimizationConfig -> IR -> OptimizationPass -> IO IR
runOptimizationPass config ir pass = do
    let passConfig = OptimizationPassConfig
            { configDebugInfo = debugInfo config
            , passName = show pass
            }
    runReaderT (optimizePass ir) passConfig

optimizePass :: IR -> ReaderT OptimizationPassConfig IO IR
optimizePass ir = do
    config <- ask
    -- Implementation here
    return ir

-- | Convert integer level to OptimizationLevel
optimizationLevelFromInt :: Int -> OptimizationLevel
optimizationLevelFromInt 0 = O0
optimizationLevelFromInt 1 = O1
optimizationLevelFromInt 2 = O2
optimizationLevelFromInt _ = O3
