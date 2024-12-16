{-# LANGUAGE OverloadedStrings #-}
module Zap.Parser.Translate
  ( translateToAST
  , TranslateError(..)
  ) where

import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad (forM_, when)
import Zap.Analysis.Lexical
import Zap.Parser.Types
import Zap.AST

data TranslateError
  = UnexpectedToken Token String
  | MissingToken String
  | InvalidIndentation Int Int
  deriving (Show, Eq)

translateToAST :: Located -> Located -> Except TranslateError Expr
translateToAST printTok strTok = do
  case (locToken printTok, locToken strTok) of
    (TWord "print", TString str) -> do
      when (locCol strTok <= locCol printTok) $
        throwError $ InvalidIndentation (locCol printTok) (locCol strTok)
      return $ Print (StrLit str)
    (TWord "print", tok) ->
      throwError $ UnexpectedToken tok "string literal"
    (tok, _) ->
      throwError $ UnexpectedToken tok "print"

translateBlock :: [Located] -> Except TranslateError Expr
translateBlock (nameTok:exprs) = case locToken nameTok of
  TWord name -> do
    let baseIndent = locCol nameTok
    forM_ exprs $ \expr ->
      when (locCol expr <= baseIndent) $
        throwError $ InvalidIndentation baseIndent (locCol expr)
    translatedExprs <- mapM translateExpr exprs
    return $ Block $ BlockScope
      { blockLabel = name
      , blockExprs = translatedExprs
      , blockResult = Nothing
      }
  tok -> throwError $ UnexpectedToken tok "block name"

translateExpr :: Located -> Except TranslateError Expr
translateExpr tok = case locToken tok of
  TString str -> return $ StrLit str
  TWord "print" -> throwError $ UnexpectedToken (locToken tok) "expression"
  TWord var -> return $ Var var
  _ -> throwError $ UnexpectedToken (locToken tok) "expression"
