{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.Allocation
  ( analyzeAllocations
  , AllocError(..)
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace

import Zap.IR.Core

data AllocError
  = InvalidAllocation IRType IRAllocStrat
  | InvalidArenaUse T.Text
  | PotentialLeak IRExpr
  | ScopedAllocationError T.Text
  | ArenaLifetimeError T.Text T.Text
  | NestedArenaError T.Text
  | CircularAllocation T.Text
  deriving (Show, Eq)

data AllocScope = AllocScope
  { scopeName :: T.Text
  , scopeAllocs :: S.Set T.Text      -- Active allocations in this scope
  , scopeArenas :: S.Set T.Text      -- Active arenas in this scope
  , parentScope :: Maybe AllocScope
  }
  deriving (Show, Eq)

data AllocState = AllocState
  { currentScope :: AllocScope
  , globalAllocs :: S.Set T.Text
  , globalArenas :: S.Set T.Text
  , allocSizes :: M.Map IRType Integer
  , tempAllocs :: S.Set T.Text
  , structDeps :: M.Map T.Text (S.Set T.Text)
  }

type AllocAnalysis = ExceptT AllocError (State AllocState)

analyzeAllocations :: IR -> Either AllocError IR
analyzeAllocations ir = evalState (runExceptT $ analyzeIR ir) initState
  where
    initState = AllocState
      { currentScope = AllocScope "global" S.empty S.empty Nothing
      , globalAllocs = S.empty
      , globalArenas = S.empty
      , allocSizes = M.empty
      , tempAllocs = S.empty
      , structDeps = M.empty
      }

analyzeIR :: IR -> AllocAnalysis IR
analyzeIR (IRProgram decls exprs) = do
  traceM "Starting IR analysis"
  mapM_ analyzeStructDeps decls
  decls' <- mapM analyzeDecl decls
  exprs' <- analyzeExprs exprs
  st <- get
  traceM $ "Final state - global arenas: " ++ show (globalArenas st)
  traceM $ "Final state - global allocs: " ++ show (globalAllocs st)
  traceM $ "Final state - temp allocs: " ++ show (tempAllocs st)
  checkGlobalLeaks
  return $ IRProgram decls' exprs'

analyzeStructDeps :: IRDecl -> AllocAnalysis ()
analyzeStructDeps (IRStruct name fields) = do
  traceM $ "Analyzing struct dependencies for: " ++ show name
  let deps = S.fromList [t | (_, IRTypeStruct t _) <- fields]
  modify $ \s -> s { structDeps = M.insert name deps (structDeps s) }
  checkCircularDeps name S.empty
analyzeStructDeps _ = return ()

checkCircularDeps :: T.Text -> S.Set T.Text -> AllocAnalysis ()
checkCircularDeps name visited = do
  traceM $ "Checking circular deps for " ++ show name ++ " visited: " ++ show visited
  when (S.member name visited) $
    throwError $ CircularAllocation name
  deps <- gets structDeps
  case M.lookup name deps of
    Nothing -> return ()
    Just depSet -> do
      let newVisited = S.insert name visited
      forM_ (S.toList depSet) $ \dep ->
        checkCircularDeps dep newVisited

analyzeDecl :: IRDecl -> AllocAnalysis IRDecl
analyzeDecl decl = case decl of
  IRFunc name params retType body -> do
    traceM $ "Analyzing function: " ++ show name
    withNewScope name $ do
      mapM_ (analyzeParam . snd) params
      body' <- analyzeExpr body
      checkScopeLeaks
      return $ IRFunc name params retType body'
  IRStruct {} -> return decl

analyzeExprs :: [IRExpr] -> AllocAnalysis [IRExpr]
analyzeExprs = go []
  where
    go acc [] = return acc
    go acc (irExpr:rest) = do
        let IRExpr meta exprNode = irExpr
        case exprNode of
            IRBlockAlloc "arena" exprs mResult -> do
                -- Register arena in global state for all subsequent expressions
                modify $ \s -> s { globalArenas = S.insert "arena" (globalArenas s) }

                -- Process inner expressions
                processedExprs <- mapM analyzeExpr exprs
                processedResult <- traverse analyzeExpr mResult

                -- Create new wrapped expression with processed contents
                let newExpr = IRExpr meta (IRBlockAlloc "arena" processedExprs processedResult)

                checkLeaksAfterExpr
                go (acc ++ [newExpr]) rest

            IRBlockAlloc name exprs mResult -> do
                -- Process block contents within new scope
                processedExprs <- withNewScope name $ do
                    exprs' <- mapM analyzeExpr exprs
                    -- Capture allocations from block scope
                    scope <- gets currentScope
                    modify $ \s -> s {
                        globalAllocs = scopeAllocs scope `S.union` globalAllocs s
                    }
                    return exprs'

                -- Process result expression if present
                processedResult <- traverse analyzeExpr mResult

                -- Create new wrapped expression with processed contents
                let newExpr = IRExpr meta (IRBlockAlloc name processedExprs processedResult)

                checkLeaksAfterExpr
                go (acc ++ [newExpr]) rest

            _ -> do
                -- For all other expressions, analyze and maintain the wrapper
                processedExpr <- analyzeExpr irExpr
                checkLeaksAfterExpr
                go (acc ++ [processedExpr]) rest

analyzeExpr :: IRExpr -> AllocAnalysis IRExpr
analyzeExpr (IRExpr meta node) = do
    -- Helper function to wrap result with updated metadata
    let wrap newNode = IRExpr meta newNode

    traceM $ "Analyzing expression: " ++ show node
    case node of
        IRBlock name exprs mResult -> do
            traceM $ "Analyzing block: " ++ show name
            let isArena = name == "arena"
            when isArena $ do
                traceM "Setting up arena block"
                modify $ \s -> s {
                    globalArenas = S.insert "arena" (globalArenas s),
                    currentScope = (currentScope s) {
                        scopeArenas = S.insert "arena" (scopeArenas (currentScope s))
                    }
                }

            exprs' <- withNewScope name $ do
                exprs'' <- mapM analyzeExpr exprs
                when (not isArena) $ do
                    scope <- gets currentScope
                    let allocs = scopeAllocs scope
                    modify $ \s -> s { globalAllocs = allocs `S.union` globalAllocs s }
                return exprs''

            mResult' <- mapM analyzeExpr mResult
            return $ wrap $ IRBlock name exprs' mResult'

        IRLetAlloc name ex strat -> do
          traceM $ "Analyzing let binding allocation: " ++ show name ++ " for expression: " ++ show ex ++ " with strategy: " ++ show strat
          validateAllocation (exprType (metadata ex)) strat
          trackAllocation name strat
          return $ wrap $ IRLetAlloc name ex strat

        IRBlockAlloc name exprs mResult -> do
            let isArena = name == "arena"
            when isArena $ do
                modify $ \s -> s {
                    currentScope = (currentScope s) { scopeArenas = S.insert "arena" (scopeArenas (currentScope s)) }
                }

            exprs' <- withNewScope name $ do
                exprs'' <- mapM analyzeExpr exprs
                scope <- gets currentScope
                let allocs = scopeAllocs scope
                modify $ \s -> s { globalAllocs = allocs `S.union` globalAllocs s }
                return exprs''

            mResult' <- mapM analyzeExpr mResult
            return $ wrap $ IRBlockAlloc name exprs' mResult'

        IRVarAlloc name strat -> do
            traceM $ "Analyzing var allocation: " ++ show name ++ " with strategy " ++ show strat
            validateAllocation (IRTypeNum IRInt32) strat
            trackAllocation name strat
            return $ wrap $ IRVarAlloc name strat

        -- Continue with other patterns...
        _ -> return $ IRExpr meta node

validateAllocation :: IRType -> IRAllocStrat -> AllocAnalysis ()
validateAllocation typ strat = do
  traceM $ "Validating allocation - type: " ++ show typ ++ ", strategy: " ++ show strat
  st <- get
  traceM $ "Current global arenas: " ++ show (globalArenas st)
  case strat of
    IRAllocStack -> case typ of
      IRTypeNum _ -> return ()
      IRTypeBool -> return ()
      IRTypeVec _ -> return ()
      _ -> throwError $ InvalidAllocation typ strat

    IRAllocArena -> do
      let hasArena = S.member "arena" (globalArenas st)
      traceM $ "Has arena: " ++ show hasArena
      unless hasArena $
        throwError $ InvalidArenaUse (scopeName (currentScope st))

    IRAllocHeap -> return ()

    IRAllocTemp -> return ()

    IRAllocCustom name -> do
      let allocs = globalAllocs st
      traceM $ "Checking custom allocator " ++ show name ++ " in allocs: " ++ show allocs
      unless (S.member name allocs) $
        throwError $ ScopedAllocationError name

    _ -> return ()

trackAllocation :: T.Text -> IRAllocStrat -> AllocAnalysis ()
trackAllocation name strat = do
  scope <- gets currentScope
  case strat of
    IRAllocHeap -> modify $ \s -> s {
      currentScope = scope {
        scopeAllocs = S.insert "_heap" $ S.insert name (scopeAllocs scope)
      }
    }
    IRAllocCustom _ -> modify $ \s -> s {
      currentScope = scope { scopeAllocs = S.insert name (scopeAllocs scope) },
      globalAllocs = S.insert name (globalAllocs s)
    }
    _ -> modify $ \s -> s {
      currentScope = scope { scopeAllocs = S.insert name (scopeAllocs scope) }
    }

analyzeParam :: IRType -> AllocAnalysis ()
analyzeParam _ = do
  scope <- gets currentScope
  modify $ \s -> s {
    currentScope = scope { scopeAllocs = S.insert "_param" (scopeAllocs scope) }
  }

withNewScope :: T.Text -> AllocAnalysis a -> AllocAnalysis a
withNewScope name action = do
  traceM $ "Creating new scope: " ++ show name
  pushScope name
  result <- action
  popScope
  return result

pushScope :: T.Text -> AllocAnalysis ()
pushScope name = do
  current <- gets currentScope
  let new = AllocScope name S.empty S.empty (Just current)
  modify $ \s -> s { currentScope = new }
  traceM $ "Pushed scope: " ++ show name

popScope :: AllocAnalysis ()
popScope = do
  current <- gets currentScope
  traceM $ "Popping scope: " ++ show (scopeName current)
  case parentScope current of
    Just parent -> modify $ \s -> s {
      currentScope = parent,
      tempAllocs = tempAllocs s `S.difference` scopeAllocs current
    }
    Nothing -> return ()

checkLeaksAfterExpr :: AllocAnalysis ()
checkLeaksAfterExpr = do
  scope <- gets currentScope
  temps <- gets tempAllocs
  forM_ (S.toList $ scopeAllocs scope) $ \alloc -> do
    when (alloc == "_heap" && not (S.member "_param" $ scopeAllocs scope)) $
      throwError $ PotentialLeak $ IRExpr
        (IRMetadata IRTypeVoid (S.singleton PureEffect) Nothing)
        (IRVar "heap_alloc")
    when (S.member alloc temps) $
      throwError $ PotentialLeak $ IRExpr
        (IRMetadata IRTypeVoid (S.singleton PureEffect) Nothing)
        (IRVar "temp_alloc")

checkScopeLeaks :: AllocAnalysis ()
checkScopeLeaks = do
  traceM "Checking scope leaks"
  checkLeaksAfterExpr

checkGlobalLeaks :: AllocAnalysis ()
checkGlobalLeaks = do
  traceM "Checking global leaks"
  checkLeaksAfterExpr
