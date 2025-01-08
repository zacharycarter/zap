{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Zap.Analysis.Semantic
  ( analyze
  , SemanticError(..)
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import qualified Data.Map.Strict as M
import Debug.Trace

import Zap.AST

data SemanticError
  = EmptyStringLiteral
  | UndefinedVariable String
  | UndefinedStruct String
  | UndefinedField String String
  | UndefinedFunction String
  | DuplicateFieldName String
  | ArgumentCountMismatch String Int Int
  | InvalidBreak String
  | RecursionInGlobalScope String
  deriving (Show, Eq)

data FuncSig = FuncSig [Type] Type
  deriving (Show, Eq)

type VarEnv = M.Map String Type
type StructEnv = M.Map String [(String, Type)]
type FuncEnv = M.Map String FuncSig
type BlockStack = [String]
type Env = (VarEnv, FuncEnv, StructEnv, BlockStack)
type SemCheck a = StateT Env (Except SemanticError) a

getInitialState :: Env
getInitialState =
  ( M.empty
  , M.fromList
    [ ("Vec2", FuncSig [TypeNum Float32, TypeNum Float32] (TypeVec (Vec2 Float32)))
    , ("Vec3", FuncSig [TypeNum Float32, TypeNum Float32, TypeNum Float32] (TypeVec (Vec3 Float32)))
    , ("Vec4", FuncSig [TypeNum Float32, TypeNum Float32, TypeNum Float32, TypeNum Float32] (TypeVec (Vec4 Float32)))
    , ("print", FuncSig [TypeAny] TypeVoid)
    ]
  , M.empty
  , []
  )

analyze :: Program -> Either SemanticError Program
analyze (Program tops) = runExcept $ evalStateT (do
    traceM "\n=== Running Semantic Analysis ==="
    mapM_ collectDeclarations tops
    processedTops <- mapM checkWellFormed tops
    return $ Program processedTops) getInitialState

collectDeclarations :: TopLevel -> SemCheck ()
collectDeclarations (TLDecl (DFunc name params retType _)) = do
    (vars, funs, structs, blocks) <- get
    let ptypes = [t | Param _ t <- params]
    when (M.member name funs) $
        throwError $ RecursionInGlobalScope name
    put (vars, M.insert name (FuncSig ptypes retType) funs, structs, blocks)
collectDeclarations (TLDecl (DStruct name fields)) = do
    (vars, funs, structs, blocks) <- get
    put (vars, funs, M.insert name fields structs, blocks)
collectDeclarations _ = return ()

checkWellFormed :: TopLevel -> SemCheck TopLevel
checkWellFormed tl@(TLExpr expr) = do
    checkExpr expr
    return tl
checkWellFormed tl@(TLDecl d) = do
    checkDecl d
    return tl
checkWellFormed tl = return tl

checkExpr :: Expr -> SemCheck ()
checkExpr = \case
    VarDecl name val -> do
        checkExpr val  -- Check the initialization value
        (vars, funs, structs, blocks) <- get
        put (M.insert name TypeAny vars, funs, structs, blocks)

    Call name args -> do
        -- Verify function exists and arity matches
        (_, funs, _, _) <- get
        case M.lookup name funs of
            Nothing -> throwError $ UndefinedFunction name
            Just (FuncSig params _) -> do
                when (length params /= length args) $
                    throwError $ ArgumentCountMismatch name (length params) (length args)
                mapM_ checkExpr args

    Var name -> do
        -- Check variable is in scope
        (vars, _, _, _) <- get
        unless (M.member name vars) $
            throwError $ UndefinedVariable name

    StrLit s -> when (null s) $ throwError EmptyStringLiteral

    BinOp _ e1 e2 -> do
        checkExpr e1
        checkExpr e2

    Let name val -> do
        checkExpr val
        (vars, funs, structs, blocks) <- get
        put (M.insert name TypeAny vars, funs, structs, blocks)

    FieldAccess expr field -> do
        checkExpr expr
        -- Check field exists in struct/vector
        (_, _, structs, _) <- get
        -- Field access validation would go here
        return ()

    _ -> return ()

checkDecl :: Decl -> SemCheck ()
checkDecl (DFunc _ params _ body) = do
    -- Create new scope with parameters
    (vars, funs, structs, blocks) <- get
    let paramVars = M.fromList [(name, typ) | Param name typ <- params]
    put (M.union paramVars vars, funs, structs, blocks)

    -- Check function body
    checkExpr body

    -- Restore original scope
    put (vars, funs, structs, blocks)

checkDecl (DStruct _ fields) = do
    -- Just verify field names are unique
    let fieldNames = map fst fields
    when (length fieldNames /= length (nub fieldNames)) $
        throwError $ DuplicateFieldName "Duplicate field names in struct"
