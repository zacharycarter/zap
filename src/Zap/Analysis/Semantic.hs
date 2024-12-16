{-# LANGUAGE FlexibleContexts #-}
module Zap.Analysis.Semantic
  ( analyze
  , SemanticError(..)
  ) where

import Control.Monad (void)
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
  | TypeMismatch Type Type
  | TypeMismatchInOp Op Type Type
  | TypeMismatchInFunction String Type Type
  | ArgumentCountMismatch String Int Int
  | InvalidBreak String
  | InvalidVectorComponents VecType [Type]
  | IndexNotInteger Type
  | ResultOutsideBlock
  | IncompatibleTypes String Type Type
  deriving (Show, Eq)

type VarEnv = M.Map String Type
type StructEnv = M.Map String [(String, Type)]
data FuncSig = FuncSig [Type] Type
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
    if M.member name funs
        then throwError $ TypeMismatchInFunction name retType retType
        else put (vars, M.insert name (FuncSig ptypes retType) funs, structs, blocks)
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
inferTypeExpr (NumLit numType _) = return $ TypeNum numType
inferTypeExpr (VecLit vecType components) = do
    componentTypes <- mapM inferTypeExpr components
    let expectedType = case vecType of
            Vec2 nt -> TypeNum nt
            Vec3 nt -> TypeNum nt
            Vec4 nt -> TypeNum nt
    let expectedCount = case vecType of
            Vec2 _ -> 2
            Vec3 _ -> 3
            Vec4 _ -> 4
    when (length componentTypes /= expectedCount) $
        throwError $ InvalidVectorComponents vecType componentTypes
    when (not $ all (== expectedType) componentTypes) $
        throwError $ InvalidVectorComponents vecType componentTypes
    return $ TypeVec vecType

inferTypeExpr (StrLit s) =
    if null s
        then throwError EmptyStringLiteral
        else return TypeString

inferTypeExpr (BoolLit _) = return TypeBool

inferTypeExpr (Var v) = do
    (vars, _, _, _) <- get
    case M.lookup v vars of
        Just t -> return t
        Nothing -> throwError $ UndefinedVariable v

inferTypeExpr (Let name val) = do
    valType <- inferTypeExpr val
    (vars, funs, structs, blocks) <- get
    put (M.insert name valType vars, funs, structs, blocks)
    return valType

inferTypeExpr (Print e) = inferTypeExpr e >> return TypeString

inferTypeExpr (Block scope) = do
    (vars, funs, structs, blocks) <- get
    put (vars, funs, structs, blockLabel scope : blocks)
    exprsTypes <- mapM inferTypeExpr (blockExprs scope)
    resultType <- case blockResult scope of
        Just result -> inferTypeExpr result
        Nothing -> if null exprsTypes
                    then return $ TypeNum Int32
                    else return $ last exprsTypes
    (vars', funs', structs', _) <- get
    put (vars', funs', structs', blocks)
    return resultType

-- Rest of inference rules for binary operations, control flow, etc...

checkBinOp :: Op -> Type -> Type -> Either SemanticError Type
checkBinOp op t1 t2 = case (op, t1, t2) of
    (Add, TypeNum n1, TypeNum n2) | n1 == n2 -> Right $ TypeNum n1
    (Add, TypeVec v1, TypeVec v2) | v1 == v2 -> Right $ TypeVec v1
    (Sub, TypeNum n1, TypeNum n2) | n1 == n2 -> Right $ TypeNum n1
    (Sub, TypeVec v1, TypeVec v2) | v1 == v2 -> Right $ TypeVec v1
    (Mul, TypeNum n1, TypeNum n2) | n1 == n2 -> Right $ TypeNum n1
    (Mul, TypeVec v1, TypeVec v2) | v1 == v2 -> Right $ TypeVec v1
    (Div, TypeNum n1, TypeNum n2) | n1 == n2 -> Right $ TypeNum n1
    (Eq, t1, t2) | t1 == t2 -> Right TypeBool
    (Lt, TypeNum n1, TypeNum n2) | n1 == n2 -> Right TypeBool
    (Gt, TypeNum n1, TypeNum n2) | n1 == n2 -> Right TypeBool
    (And, TypeBool, TypeBool) -> Right TypeBool
    (Or, TypeBool, TypeBool) -> Right TypeBool
    _ -> Left $ TypeMismatchInOp op t1 t2
