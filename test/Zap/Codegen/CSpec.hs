{-# LANGUAGE OverloadedStrings #-}

module Zap.Codegen.CSpec (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import Test.Hspec

import Zap.IR
import Zap.Codegen.C

spec :: Spec
spec = do
  describe "C Code Generation" $ do
    describe "Literal code generation" $ do
      it "generates valid C numeric literals" $ do
          let program = IRProgram
                [ ( IRFuncDecl "main" [] IRTypeVoid
                    (IRBlock "entry"
                      [ (IRVarDecl "x" IRTypeInt32 (IRLit (IRInt32Lit 5)), testMeta)
                      , (IRAssign "x" (IRLit (IRInt32Lit 3)), testMeta)
                      ])
                  , testMeta)
                ]
          case generateC program of
              Right code -> do
                  T.isInfixOf "int x = 5" code `shouldBe` True
                  T.isInfixOf "x = 3" code `shouldBe` True
              Left err -> expectationFailure $ show err

      where
        testMeta = IRMetadata
          { metaType = IRTypeVar (TypeVar 0)  -- Use type variable instead of concrete type
          , metaEffects = S.singleton PureEffect
          , metaSourcePos = Nothing
          , metaLiteralType = Nothing
          }
