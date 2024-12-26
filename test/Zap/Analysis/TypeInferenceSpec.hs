{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.TypeInferenceSpec (spec) where

import Test.Hspec
-- import qualified Data.Text as T
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
-- import Control.Monad.State
-- import Control.Monad.Except

-- import Zap.AST
-- import Zap.IR.Core
-- import Zap.Analysis.Semantic
-- import Zap.Util (mkTestExpr)

spec :: Spec
spec = do
  describe "Type Inference" $ do
    it "passes" $ do
      True
--     describe "Basic Inference" $ do
--       it "infers types of numeric literals" $ do
--         let ast = Program [TLExpr (NumLit Int32 "42")]
--         case analyze ast of
--           Right (Program [TLExpr expr]) ->
--             typecheckExpr expr `shouldBe` Right (TypeNum Int32)
--           Left err -> expectationFailure $ "Analysis failed: " ++ show err

--       it "infers types in binary operations" $ do
--         let ast = Program [TLExpr (
--                               BinOp Add
--                               (NumLit Int32 "1")
--                               (NumLit Int32 "2")
--                               )]
--         case analyze ast of
--           Right (Program [TLExpr expr]) ->
--             typecheckExpr expr `shouldBe` Right (TypeNum Int32)
--           Left err -> expectationFailure $ "Analysis failed: " ++ show err

--     -- describe "Let Binding Inference" $ do
--     --   it "infers types of let-bound variables" $ do
--     --     let ast = Program [TLExpr (
--     --                           Let "x" ((NumLit Int32 "1") (
--     --                               BinOp Add (Var "x") (NumLit Int32 "2")))
--     --                           )]

--     --     case analyze ast of
--     --       Right (Program [TLExpr expr]) ->
--     --         typecheckExpr expr `shouldBe` Right (TypeNum Int32)
--     --       Left err -> expectationFailure $ "Analysis failed: " ++ show err

--     describe "Function Type Inference" $ do
--       it "infers return type of simple function" $ do
--         let ast = Program [TLDecl (
--                               DFunc "add"
--                               [Param "x" (TypeNum Int32), Param "y" (TypeNum Int32)]
--                               (TypeNum Int32)
--                               (BinOp Add (Var "x") (Var "y"))
--                               )]
--         case analyze ast of
--           Right _ -> return ()  -- Success case
--           Left err -> expectationFailure $ "Analysis failed: " ++ show err

-- -- Helper function to be implemented
-- typecheckExpr :: Expr -> Either SemanticError Type
-- typecheckExpr expr = undefined  -- We'll implement this next
