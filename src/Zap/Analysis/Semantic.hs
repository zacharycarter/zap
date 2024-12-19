{-# LANGUAGE FlexibleContexts #-}
module Zap.Analysis.Semantic
  ( analyze
  , SemanticError(..)
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M
import Control.Monad (when)
import Zap.AST

data SemanticError
  = EmptyStringLiteral
  | UndefinedVariable String
  | UndefinedStruct String
  | UndefinedField String String
  | UndefinedFunction String
  | TypeMismatch Type Type
  | TypeMismatchInOp Op Type Type
  | TypeMismatchInFunction String Type Type
  | ArgumentCountMismatch String Int Int
  | InvalidBreak String
  | InvalidVectorComponents VecType [Type]
  | IndexNotInteger Type
  | ResultOutsideBlock
  | IncompatibleTypes String Type Type
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

analyze :: Program -> Either SemanticError Program
analyze (Program tops) = runExcept $ evalStateT (checkProgram tops) (M.empty, M.empty, M.empty, [])

checkProgram :: [TopLevel] -> SemCheck Program
checkProgram tops = do
    mapM_ collectDeclarations tops
    mapM_ checkTopLevel tops
    return $ Program tops

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

checkTopLevel :: TopLevel -> SemCheck ()
checkTopLevel (TLExpr e) = void $ inferTypeExpr e
checkTopLevel (TLDecl d) = checkDecl d

checkDecl :: Decl -> SemCheck ()
checkDecl (DFunc name params retType body) = do
    (vars, funs, structs, blocks) <- get
    let paramMap = M.fromList [(n, t) | Param n t <- params]
    put (M.union paramMap vars, funs, structs, blocks)
    bodyType <- inferTypeExpr body
    when (bodyType /= retType) $
        throwError $ TypeMismatchInFunction name retType bodyType
    put (vars, funs, structs, blocks)
checkDecl (DStruct _ _) = return ()

inferTypeExpr :: Expr -> SemCheck Type
inferTypeExpr expr = case expr of
    StrLit s ->
        if null s
            then throwError EmptyStringLiteral
            else return TypeString

    NumLit numType _ -> return $ TypeNum numType

    Var v -> do
        (vars, _, _, _) <- get
        case M.lookup v vars of
            Just t -> return t
            Nothing -> throwError $ UndefinedVariable v

    Call name args -> do
        (_, funs, _, _) <- get
        case M.lookup name funs of
            Nothing -> throwError $ UndefinedFunction name
            Just (FuncSig paramTypes retType) -> do
                when (length args /= length paramTypes) $
                    throwError $ ArgumentCountMismatch name (length paramTypes) (length args)
                argTypes <- mapM inferTypeExpr args
                zipWithM_ (\expected actual ->
                    unless (expected == actual) $
                        throwError $ TypeMismatchInFunction name expected actual)
                    paramTypes argTypes
                return retType

    BinOp op e1 e2 -> do
        t1 <- inferTypeExpr e1
        t2 <- inferTypeExpr e2
        case (op, t1, t2) of
            -- Vector addition
            (Add, TypeVec v1, TypeVec v2) | v1 == v2 ->
                return $ TypeVec v1

            -- Vector dot product
            (Dot, TypeVec v1, TypeVec v2) | v1 == v2 ->
                case v1 of
                    Vec2 numType -> return $ TypeNum numType
                    Vec3 numType -> return $ TypeNum numType
                    Vec4 numType -> return $ TypeNum numType

            -- Existing numeric operations
            (Add, TypeNum n1, TypeNum n2) | n1 == n2 ->
                return $ TypeNum n1
            (Sub, TypeNum n1, TypeNum n2) | n1 == n2 ->
                return $ TypeNum n1
            (Mul, TypeNum n1, TypeNum n2) | n1 == n2 ->
                return $ TypeNum n1
            (Div, TypeNum n1, TypeNum n2) | n1 == n2 ->
                return $ TypeNum n1

            -- Type mismatch error cases
            (Add, TypeVec _, TypeVec _) ->
                throwError $ TypeMismatchInOp op t1 t2
            (Dot, TypeVec _, TypeVec _) ->
                throwError $ TypeMismatchInOp op t1 t2
            _ -> throwError $ TypeMismatchInOp op t1 t2

    FieldAccess expr field -> do
        exprType <- inferTypeExpr expr
        case exprType of
            TypeVec vecType -> case (vecType, field) of
                (_, "x") -> return $ TypeNum $ getVecNumType vecType
                (Vec2 nt, "y") -> return $ TypeNum nt
                (Vec3 nt, "y") -> return $ TypeNum nt
                (Vec3 nt, "z") -> return $ TypeNum nt
                (Vec4 nt, "y") -> return $ TypeNum nt
                (Vec4 nt, "z") -> return $ TypeNum nt
                (Vec4 nt, "w") -> return $ TypeNum nt
                _ -> throwError $ UndefinedField (show vecType) field
            _ -> throwError $ UndefinedField (show exprType) field

    VecLit vecType components -> do
        componentTypes <- mapM inferTypeExpr components
        let expectedCount = case vecType of
                Vec2 _ -> 2
                Vec3 _ -> 3
                Vec4 _ -> 4
        let actualCount = length components

        when (actualCount /= expectedCount) $
            throwError $ InvalidVectorComponents vecType componentTypes

        let checkType = TypeNum $ case vecType of
                Vec2 numType -> numType
                Vec3 numType -> numType
                Vec4 numType -> numType

        forM_ componentTypes $ \compType ->
            unless (compType == checkType) $
                throwError $ InvalidVectorComponents vecType componentTypes

        return $ TypeVec vecType

    Let name expr -> do
        exprType <- inferTypeExpr expr
        (vars, funs, structs, blocks) <- get
        put (M.insert name exprType vars, funs, structs, blocks)
        return exprType

    BoolLit _ -> return TypeBool

    Print e -> do
        eType <- inferTypeExpr e
        case eType of
            TypeString -> return TypeString
            TypeNum numType -> return TypeString
            TypeVec vecType -> return TypeString
            _ -> throwError $ TypeMismatch TypeString eType

    Block scope -> do
        -- Process all expressions in the block
        mapM_ inferTypeExpr (blockExprs scope)

        -- Get block's type from result expression or last expression
        case blockResult scope of
            Just result -> inferTypeExpr result
            Nothing -> if null (blockExprs scope)
                        then return $ TypeNum Int32  -- Default type for empty blocks
                        else inferTypeExpr (last (blockExprs scope))

    _ -> throwError $ IncompatibleTypes "Unsupported expression type" TypeBool TypeBool

-- Helper function to extract numeric type from vector type
getVecNumType :: VecType -> NumType
getVecNumType (Vec2 nt) = nt
getVecNumType (Vec3 nt) = nt
getVecNumType (Vec4 nt) = nt
