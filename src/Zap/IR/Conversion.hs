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
import Debug.Trace

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
  traceM "\n=== Starting IR Conversion ==="
  traceM $ "Converting program with " ++ show (length topLevels) ++ " top-level expressions"
  decls <- sequence [convertDecl d | TLDecl d <- topLevels]
  exprs <- concat <$> sequence [convertTopLevel t | t@(TLExpr _) <- topLevels]
  return $ IRProgram decls exprs

convertTopLevel :: TopLevel -> IRConverter [IRExpr]
convertTopLevel (TLExpr expr) = do
  traceM $ "\nConverting top-level expression: " ++ show expr
  converted <- convertExpr expr
  return [converted]
convertTopLevel (TLDecl _) =
  return []

convertDecl :: Decl -> IRConverter IRDecl
convertDecl (DFunc name params retType body) = do
  traceM $ "\nConverting function declaration: " ++ name
  convertedParams <- mapM convertParam params
  traceM "Converting function body"
  convertedBody <- convertExpr body
  return $ IRFunc
    (T.pack name)
    convertedParams
    (convertType retType)
    convertedBody
convertDecl (DStruct name fields) = do
  traceM $ "\nConverting struct declaration: " ++ name
  return $ IRStruct
    (T.pack name)
    [(T.pack fname, convertType ftype) | (fname, ftype) <- fields]

convertParam :: Param -> IRConverter (T.Text, IRType)
convertParam (Param name typ) = do
  traceM $ "Converting parameter: " ++ name ++ " :: " ++ show typ
  return (T.pack name, convertType typ)

convertExpr :: Expr -> IRConverter IRExpr
convertExpr expr = do
    traceM $ "\nConverting expression: " ++ show expr
    case expr of
        StrLit s -> do
            traceM "Converting string literal"
            return $ IRString (T.pack s)

        NumLit numType val -> do
            traceM $ "Converting numeric literal: " ++ val
            return $ IRNum (convertNumType numType) (T.pack val)

        BinOp op e1 e2 -> do
            traceM $ "Converting binary operation: " ++ show op
            converted1 <- convertExpr e1
            traceM $ "Left operand converted to: " ++ show converted1
            converted2 <- convertExpr e2
            traceM $ "Right operand converted to: " ++ show converted2

            type1 <- getExpressionType converted1
            type2 <- getExpressionType converted2
            traceM $ "Types for binary operation: " ++ show type1 ++ " and " ++ show type2

            case (op, type1, type2) of
                (Add, IRTypeVec vt1, IRTypeVec vt2) | vt1 == vt2 -> do
                    traceM "Matched vector addition"
                    return $ IRBinOp IRAdd converted1 converted2
                (Add, IRTypeNum t1, IRTypeNum t2) | t1 == t2 -> do
                    traceM "Matched numeric addition"
                    return $ IRBinOp IRAdd converted1 converted2
                (Sub, IRTypeNum t1, IRTypeNum t2) | t1 == t2 -> do
                    traceM "Matched numeric subtraction"
                    return $ IRBinOp IRSub converted1 converted2
                (Mul, IRTypeNum t1, IRTypeNum t2) | t1 == t2 -> do
                    traceM "Matched numeric multiplication"
                    return $ IRBinOp IRMul converted1 converted2
                (Div, IRTypeNum t1, IRTypeNum t2) | t1 == t2 -> do
                    traceM "Matched numeric division"
                    return $ IRBinOp IRDiv converted1 converted2
                _ -> do
                    traceM $ "Unmatched operation types: " ++ show type1 ++ " and " ++ show type2
                    throwError $ UnsupportedExpression "Operation only supported for matching types"

        Print e -> do
            traceM "Converting print expression"
            converted <- convertExpr e
            traceM $ "Print expression converted to: " ++ show converted
            return $ IRPrint converted

        Block scope -> do
            traceM $ "Converting block: " ++ blockLabel scope
            blockId <- freshBlockId
            bodyExprs <- mapM convertExpr (blockExprs scope)
            resultExpr <- case blockResult scope of
                Just result -> Just <$> convertExpr result
                Nothing -> return Nothing
            return $ IRBlockAlloc
                (T.pack $ blockLabel scope)
                bodyExprs
                resultExpr

        Break label -> do
            traceM $ "Converting break statement for label: " ++ label
            return $ IRBreak (T.pack label)

        Result e -> do
            traceM "Converting result expression"
            converted <- convertExpr e
            return $ IRResult converted

        Var name -> do
            traceM $ "Converting variable reference: " ++ name
            state <- get
            case M.lookup name (varEnvironment state) of
                Just varType -> do
                    traceM $ "Found variable type: " ++ show varType
                    modify $ \s -> s { varEnvironment = M.insert name varType (varEnvironment s) }
                    return $ IRVar (T.pack name)
                Nothing -> do
                    traceM $ "Variable type not found in environment"
                    return $ IRVar (T.pack name)

        VecLit vecType components -> do
            traceM $ "Converting vector literal: " ++ show vecType
            convertedComps <- mapM convertExpr components
            let varType = IRTypeVec (convertVecType vecType)
            return $ IRVec (convertVecType vecType) convertedComps

        Let name val -> do
            traceM $ "Converting let binding: " ++ name
            convertedVal <- convertExpr val
            valType <- getExpressionType convertedVal
            traceM $ "Binding type: " ++ show valType
            modify $ \s -> s { varEnvironment = M.insert name valType (varEnvironment s) }
            return $ IRLetAlloc (T.pack name) convertedVal IRAllocDefault

        BoolLit b -> do
            traceM $ "Converting boolean literal: " ++ show b
            return $ IRBool b

        _ -> do
            traceM $ "Encountered unsupported expression type: " ++ show expr
            throwError $ UnsupportedExpression "Unsupported expression type"

getExpressionType :: IRExpr -> IRConverter IRType
getExpressionType expr = do
    state <- get
    case expr of
        IRVar name -> case M.lookup (T.unpack name) (varEnvironment state) of
            Just t -> return t
            Nothing -> throwError $ UnknownVariable (T.unpack name)
        IRVec vt _ -> return $ IRTypeVec vt
        IRNum nt _ -> return $ IRTypeNum nt
        IRBool _ -> return IRTypeBool
        IRString _ -> return IRTypeString
        _ -> throwError $ InvalidType "Unable to determine expression type"

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
