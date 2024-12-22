{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.CFGSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Zap.IR.Core
import Zap.Analysis.CFG

spec :: Spec
spec = describe "Control Flow Graph Analysis" $ do
  describe "buildCFG" $ do
    it "returns EmptyProgram error for empty input" $
      buildCFG [] `shouldBe` Left EmptyProgram

    it "creates single block for straight-line code" $ do
      let prog =
            [ IRNum IRInt32 "1"
            , IRNum IRInt32 "2"
            ]

      case buildCFG prog of
        Left err -> expectationFailure $ "Failed to build CFG: " ++ show err
        Right cfg -> do
          -- Should have exactly one node
          M.size (cfgNodes cfg) `shouldBe` 1

          -- Entry and exit should be the same node for single block
          cfgEntry cfg `shouldBe` cfgExit cfg

          -- Check node contents
          case M.lookup (cfgEntry cfg) (cfgNodes cfg) of
            Nothing -> expectationFailure "Entry node not found in CFG"
            Just node -> do
              length (blockStmts $ nodeBlock node) `shouldBe` 2
              blockTerminator (nodeBlock node) `shouldBe` Nothing
              S.size (successors node) `shouldBe` 0
              S.size (predecessors node) `shouldBe` 0

    it "creates proper blocks and edges for if/else" $ do
      let prog =
            [ IRNum IRInt32 "1"
            , IRIf (IRBool True)
                (IRNum IRInt32 "2")
                (IRNum IRInt32 "3")
            , IRNum IRInt32 "4"
            ]

      case buildCFG prog of
        Left err -> expectationFailure $ "Failed to build CFG: " ++ show err
        Right cfg -> do
          -- Should have 4 blocks: entry, then, else, exit
          M.size (cfgNodes cfg) `shouldBe` 4

          let entryId = cfgEntry cfg
              exitId = cfgExit cfg

          -- Get entry node and verify it points to then/else blocks
          case M.lookup entryId (cfgNodes cfg) of
            Nothing -> expectationFailure "Entry node not found"
            Just entryNode -> do
              length (blockStmts $ nodeBlock entryNode) `shouldBe` 1  -- First number
              case blockTerminator (nodeBlock entryNode) of
                Just (IRIf {}) -> return ()
                _ -> expectationFailure "Entry should terminate with if"
              S.size (successors entryNode) `shouldBe` 2  -- Points to then and else

          -- Verify exit node has both then/else as predecessors
          case M.lookup exitId (cfgNodes cfg) of
            Nothing -> expectationFailure "Exit node not found"
            Just exitNode -> do
              length (blockStmts $ nodeBlock exitNode) `shouldBe` 1  -- Last number
              blockTerminator (nodeBlock exitNode) `shouldBe` Nothing
              S.size (predecessors exitNode) `shouldBe` 2
