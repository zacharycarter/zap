{-# LANGUAGE OverloadedStrings #-}
module Zap.IR.Conversion
  ( convertToIR
  , IRConversionError(..)
  , convertType
  , convertNumType
  , convertVecType
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.State

import Zap.AST
import Zap.IR.Core

data IRConversionError
  = UnsupportedExpression String
  | InvalidType String
  | UnknownVariable String
  deriving (Show, Eq)

type IRConverter = ExceptT IRConversionError (State IRGenState)

data IRGenState = IRGenState
  { tempCounter :: Int
  , blockCounter :: Int
  , varEnvironment :: M.Map String IRType
  }

initialState :: IRGenState
initialState = IRGenState 0 0 M.empty

convertToIR :: Program -> Either IRConversionError IR
convertToIR program =
  evalState (runExceptT $ convertProgram program) initialState

convertProgram :: Program -> IRConverter IR
convertProgram (Program topLevels) = do
  decls <- sequence [convertDecl d | TLDecl d <- topLevels]
  exprs <- concat <$> sequence [convertTopLevel t | t@(TLExpr _) <- topLevels]
  return $ IRProgram decls exprs

convertTopLevel :: TopLevel -> IRConverter [IRExpr]
convertTopLevel (TLExpr expr) = do
  converted <- convertExpr expr
  return [converted]
convertTopLevel (TLDecl _) =
  return []

convertDecl :: Decl -> IRConverter IRDecl
convertDecl (DFunc name params retType body) = do
  convertedParams <- mapM convertParam params
  convertedBody <- convertExpr body
  return $ IRFunc
    (T.pack name)
    convertedParams
    (convertType retType)
    convertedBody
convertDecl (DStruct name fields) =
  return $ IRStruct
    (T.pack name)
    [(T.pack fname, convertType ftype) | (fname, ftype) <- fields]

convertParam :: Param -> IRConverter (T.Text, IRType)
convertParam (Param name typ) =
  return (T.pack name, convertType typ)

convertExpr :: Expr -> IRConverter IRExpr
convertExpr expr = case expr of
  StrLit s ->
    return $ IRString (T.pack s)

  NumLit numType val ->
    return $ IRNum (convertNumType numType) (T.pack val)

  BinOp op e1 e2 -> do
    converted1 <- convertExpr e1
    converted2 <- convertExpr e2
    case (op, converted1, converted2) of
      (Add, IRNum t1 _, IRNum t2 _) | t1 == t2 ->
        return $ IRBinOp IRAdd converted1 converted2
      (Sub, IRNum t1 _, IRNum t2 _) | t1 == t2 ->
        return $ IRBinOp IRSub converted1 converted2
      (Mul, IRNum t1 _, IRNum t2 _) | t1 == t2 ->
        return $ IRBinOp IRMul converted1 converted2
      (Div, IRNum t1 _, IRNum t2 _) | t1 == t2 ->
        return $ IRBinOp IRDiv converted1 converted2
      (Add, _, _) ->
        throwError $ UnsupportedExpression "Addition only supported for matching numeric types"
      (Sub, _, _) ->
        throwError $ UnsupportedExpression "Subtraction only supported for matching numeric types"
      (Mul, _, _) ->
        throwError $ UnsupportedExpression "Multiplication only supported for matching numeric types"
      (Div, _, _) ->
        throwError $ UnsupportedExpression "Division only supported for matching numeric types"
      _ -> throwError $ UnsupportedExpression "Unsupported binary operator"

  Print e -> do
    converted <- convertExpr e
    return $ IRPrint converted

  Block scope -> do
    blockId <- freshBlockId
    bodyExprs <- mapM convertExpr (blockExprs scope)
    resultExpr <- case blockResult scope of
      Just result -> Just <$> convertExpr result
      Nothing -> return Nothing
    return $ IRBlockAlloc
      (T.pack $ blockLabel scope)
      bodyExprs
      resultExpr

  Break label ->
    return $ IRBreak (T.pack label)

  Result e -> do
    converted <- convertExpr e
    return $ IRResult converted

  Var name -> do
    state <- get
    case M.lookup name (varEnvironment state) of
      Nothing -> throwError $ UnknownVariable name
      Just _ -> return $ IRVar (T.pack name)

  If cond thenExpr elseExpr -> do
    convertedCond <- convertExpr cond
    convertedThen <- convertExpr thenExpr
    convertedElse <- convertExpr elseExpr

    case convertedCond of
      IRBool _ -> return $ IRIf convertedCond convertedThen convertedElse
      _ -> throwError $ InvalidType "If condition must evaluate to a boolean type"

  BoolLit b ->
    return $ IRBool b
 
  _ -> throwError $ UnsupportedExpression "Unsupported expression type"

convertType :: Type -> IRType
convertType (TypeNum numType) = IRTypeNum $ convertNumType numType
convertType (TypeVec vecType) = IRTypeVec $ convertVecType vecType
convertType TypeString = IRTypeString
convertType TypeBool = IRTypeBool
convertType (TypeStruct name fields) =
  IRTypeStruct (T.pack name) [(T.pack n, convertType t) | (n, t) <- fields]
convertType (TypeArray elemType) =
  IRTypeArray (convertType elemType)

convertNumType :: NumType -> IRNumType
convertNumType Int32 = IRInt32
convertNumType Int64 = IRInt64
convertNumType Float32 = IRFloat32
convertNumType Float64 = IRFloat64

convertVecType :: VecType -> IRVecType
convertVecType (Vec2 nt) = IRVec2 (convertNumType nt)
convertVecType (Vec3 nt) = IRVec3 (convertNumType nt)
convertVecType (Vec4 nt) = IRVec4 (convertNumType nt)

freshBlockId :: IRConverter T.Text
freshBlockId = do
  state <- get
  let counter = blockCounter state
  put state { blockCounter = counter + 1 }
  return $ T.pack $ "block_" ++ show counter
