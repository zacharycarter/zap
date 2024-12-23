{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.CFGSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (forM_)

import Zap.IR.Core
import Zap.Analysis.CFG
import Zap.Util (mkTestExpr)

spec :: Spec
spec = describe "Control Flow Graph Analysis" $ do
    it "returns EmptyProgram error for empty input" $
        buildCFG [] `shouldBe` Left EmptyProgram

    it "creates single block for straight-line code" $ do
        let prog =
              [ mkTestExpr $ IRNum IRInt32 "1"
              , mkTestExpr $ IRNum IRInt32 "2"
              ]

        case buildCFG prog of
            Left err -> expectationFailure $ "Failed to build CFG: " ++ show err
            Right cfg -> do
                M.size (cfgNodes cfg) `shouldBe` 1
                cfgEntry cfg `shouldBe` cfgExit cfg

                case M.lookup (cfgEntry cfg) (cfgNodes cfg) of
                    Nothing -> expectationFailure "Entry node not found in CFG"
                    Just node -> do
                        length (blockStmts $ nodeBlock node) `shouldBe` 2
                        blockTerminator (nodeBlock node) `shouldBe` Nothing
                        S.size (successors node) `shouldBe` 0
                        S.size (predecessors node) `shouldBe` 0

    it "creates proper blocks and edges for if/else" $ do
        let prog =
              [ mkTestExpr $ IRNum IRInt32 "1"
              , mkTestExpr $ IRIf
                    (mkTestExpr $ IRBool True)
                    (mkTestExpr $ IRNum IRInt32 "2")
                    (mkTestExpr $ IRNum IRInt32 "3")
              , mkTestExpr $ IRNum IRInt32 "4"
              ]

        case buildCFG prog of
            Left err -> expectationFailure $ "Failed to build CFG: " ++ show err
            Right cfg -> do
                M.size (cfgNodes cfg) `shouldBe` 4

                let entryId = cfgEntry cfg
                    exitId = cfgExit cfg

                case M.lookup entryId (cfgNodes cfg) of
                    Nothing -> expectationFailure "Entry node not found"
                    Just entryNode -> do
                        length (blockStmts $ nodeBlock entryNode) `shouldBe` 1
                        case blockTerminator (nodeBlock entryNode) of
                            Just (IRExpr _ IRIf{}) -> pure ()
                            _ -> expectationFailure "Entry should terminate with if"
                        S.size (successors entryNode) `shouldBe` 2

                case M.lookup exitId (cfgNodes cfg) of
                    Nothing -> expectationFailure "Exit node not found"
                    Just exitNode -> do
                        length (blockStmts $ nodeBlock exitNode) `shouldBe` 1
                        blockTerminator (nodeBlock exitNode) `shouldBe` Nothing
                        S.size (predecessors exitNode) `shouldBe` 2
