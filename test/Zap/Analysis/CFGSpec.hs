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
