-- {-# LANGUAGE OverloadedStrings #-}
-- module Zap.Compiler
--   ( compile
--   , CompileError(..)
--   ) where

-- import qualified Data.Text as T
-- import Control.Monad.Except

-- import Zap.AST (Program(..), TopLevel(..), Expr(..))
-- import Zap.Parser.Program (parseProgram)
-- import Zap.Analysis.Semantic (analyze)
-- import Zap.Codegen.C (generateC)
-- import Zap.IR.Core

-- data CompileError
--   = ParseError String
--   | TranslateError String
--   | SemanticError String
--   | CodegenError String
--   deriving (Show, Eq)

-- compile :: T.Text -> Either CompileError T.Text
-- compile source = do
--   -- Parse the source into an AST expression directly
--   ast <- mapLeft (ParseError . show) $ parseProgram source

--   -- Wrap the expression in a program for analysis
--   analyzed <- mapLeft (SemanticError . show) $ analyze $ Program [TLExpr ast]

--   -- Convert to IR and generate code
--   let ir = convertToIR analyzed
--   mapLeft (CodegenError . show) $ generateC ir

-- -- Helper to map Left values
-- mapLeft :: (a -> b) -> Either a c -> Either b c
-- mapLeft f (Left x) = Left (f x)
-- mapLeft _ (Right x) = Right x

-- -- Minimal IR conversion
-- convertToIR :: Program -> IR
-- convertToIR (Program tops) = IRProgram [] (map convertTopLevel tops)
--   where
--     convertTopLevel (TLExpr e) = case e of
--       Print (StrLit s) -> IRPrint $ IRString (T.pack s)
--       _ -> error "Unsupported top-level expression"
module Zap.Compiler () where
