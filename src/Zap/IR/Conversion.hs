{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zap.IR.Conversion
  ( convertToIR
  , IRConversionError(..)
  , convertType
  , convertNumType
  , convertVecType
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Monad (forM, forM_, when)
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
convertToIR program = evalState (runExceptT $ convertProgram program) initialState

convertProgram :: Program -> IRConverter IR
convertProgram (Program tops) = do
    traceM "\n=== Starting IR Conversion ==="
    traceM $ "Converting program with " ++ show (length tops) ++ " top-level expressions"

    -- Process type declarations first to build environment
    forM_ tops $ \case
        TLType name typ@(TypeStruct sName fields) -> do
            let irFields = [(T.pack fn, convertType ft) | (fn, ft) <- fields]
            modify $ \s -> s { varEnvironment = M.insert sName (IRTypeStruct (T.pack sName) irFields) (varEnvironment s) }
        _ -> return ()

    -- Convert declarations
    decls <- mapM convertDecl [d | TLDecl d <- tops]

    -- Convert all TLType struct definitions into IRStruct declarations
    let structDecls = [ IRStruct (T.pack name) [(T.pack fn, convertType ft) | (fn,ft) <- fields]
                      | TLType name (TypeStruct sName fields) <- tops, name == sName ]

    -- Convert expressions
    exprs <- concat <$> sequence [convertTopLevel t | t@(TLExpr _) <- tops]

    traceM "Finished converting all top-level expressions"
    return $ IRProgram (decls ++ structDecls) exprs

convertTopLevel :: TopLevel -> IRConverter [IRExpr]
convertTopLevel (TLExpr expr) = do
    traceM $ "\nConverting top-level expression: " ++ show expr
    converted <- convertExpr expr
    return [converted]
convertTopLevel _ = return []

convertDecl :: Decl -> IRConverter IRDecl
convertDecl (DFunc name params retType body) = do
    traceM $ "Converting function declaration: " ++ name
    convertedBody <- convertExpr body
    return $ IRFunc (T.pack name)
                    [(T.pack n, convertType t) | Param n t <- params]
                    (convertType retType)
                    convertedBody
convertDecl (DStruct name fields) = do
    traceM $ "Converting struct declaration: " ++ name
    return $ IRStruct (T.pack name) [(T.pack fname, convertType ftype) | (fname, ftype) <- fields]

isVectorType :: IRType -> Bool
isVectorType (IRTypeVec _) = True
isVectorType (IRTypeStruct name _) | name == "Vec3" = True
isVectorType _ = False

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

        BoolLit b -> do
            traceM $ "Converting boolean literal: " ++ show b
            return $ IRBool b

        If cond thenExpr elseExpr -> do
            traceM "Converting if expression"
            convertedCond <- convertExpr cond
            convertedThen <- convertExpr thenExpr
            convertedElse <- convertExpr elseExpr
            return $ IRIf convertedCond convertedThen convertedElse

        BinOp op e1 e2 -> do
            traceM $ "Converting binary operation: " ++ show op
            converted1 <- convertExpr e1
            traceM $ "Left operand converted to: " ++ show converted1
            converted2 <- convertExpr e2
            traceM $ "Right operand converted to: " ++ show converted2
            st <- get
            let mt1 = getExprType (varEnvironment st) converted1
                mt2 = getExprType (varEnvironment st) converted2

            traceM $ "Operand types: " ++ show mt1 ++ ", " ++ show mt2

            case (op, mt1, mt2) of
                (Add, Just t1, Just t2)
                    | isVectorType t1 && isVectorType t2 -> do
                        traceM "Processing vector addition"
                        return $ IRBinOp IRAdd converted1 converted2
                    | otherwise -> case (t1, t2) of
                        (IRTypeNum n1, IRTypeNum n2) | n1 == n2 ->
                            return $ IRBinOp IRAdd converted1 converted2
                        _ -> throwError $ InvalidType "Invalid operand types for '+'"

                (Sub, Just t1, Just t2)
                    | isVectorType t1 && isVectorType t2 ->
                        return $ IRBinOp IRSub converted1 converted2
                    | otherwise -> case (t1, t2) of
                        (IRTypeNum n1, IRTypeNum n2) | n1 == n2 ->
                            return $ IRBinOp IRSub converted1 converted2
                        _ -> throwError $ InvalidType "Invalid operand types for '-'"

                (Mul, Just t1, Just t2) -> case (t1, t2) of
                    (IRTypeNum n1, IRTypeNum n2) | n1 == n2 ->
                        return $ IRBinOp IRMul converted1 converted2
                    _ -> throwError $ InvalidType "Invalid operand types for '*'"

                (Div, Just t1, Just t2) -> case (t1, t2) of
                    (IRTypeNum n1, IRTypeNum n2) | n1 == n2 ->
                        return $ IRBinOp IRDiv converted1 converted2
                    _ -> throwError $ InvalidType "Invalid operand types for '/'"

                (Dot, Just t1, Just t2)
                    | isVectorType t1 && isVectorType t2 ->
                        return $ IRBinOp IRDot converted1 converted2
                    | otherwise ->
                        throwError $ InvalidType "Invalid operand types for dot product"

                _ -> throwError $ InvalidType "Unsupported operation"

        Block scope -> do
            traceM $ "Converting block: " ++ blockLabel scope
            exprs' <- mapM convertExpr (blockExprs scope)
            mResult' <- mapM convertExpr (blockResult scope)
            traceM $ "Block converted with exprs: " ++ show exprs' ++ " and result: " ++ show mResult'
            return $ IRBlockAlloc (T.pack $ blockLabel scope) exprs' mResult'

        Result expr -> do
            traceM "Converting result expression"
            converted <- convertExpr expr
            return $ IRResult converted

        If cond thenExpr elseExpr -> do
            traceM "Converting if expression"
            convertedCond <- convertExpr cond
            convertedThen <- convertExpr thenExpr
            convertedElse <- convertExpr elseExpr
            return $ IRIf convertedCond convertedThen convertedElse

        StructLit name fields -> do
            traceM $ "Converting struct literal: " ++ name
            convertedFields <- forM fields $ \(fname, fexpr) -> do
                cf <- convertExpr fexpr
                traceM $ "Field " ++ fname ++ " converted to " ++ show cf
                return (T.pack fname, cf)
            traceM $ "Struct literal fields converted: " ++ show convertedFields
            return $ IRStructLit (T.pack name) convertedFields

        FieldAccess expr field -> do
            traceM $ "Converting field access: " ++ field
            convertedExpr <- convertExpr expr
            traceM $ "Base expression converted: " ++ show convertedExpr
            return $ IRFieldAccess convertedExpr (T.pack field)

        Call "print" [e] -> do
            traceM "Converting print call"
            convertedE <- convertExpr e
            return $ IRPrint convertedE

        Call name args -> do
            traceM $ "Converting function call: " ++ name
            convertedArgs <- mapM convertExpr args

            -- Check if it's a struct constructor
            state <- get
            case M.lookup name (varEnvironment state) of
                Just (IRTypeStruct sName fields) -> do
                    -- It's a struct constructor
                    when (length args /= length fields) $
                        throwError $ InvalidType "Wrong number of arguments for struct constructor"
                    return $ IRStructLit sName (zip (map fst fields) convertedArgs)

                -- Handle other cases (vectors, regular functions) as before
                _ -> case name of
                    "Vec2" -> return $ IRVec (IRVec2 IRFloat32) convertedArgs
                    "Vec3" -> return $ IRVec (IRVec3 IRFloat32) convertedArgs
                    "Vec4" -> return $ IRVec (IRVec4 IRFloat32) convertedArgs
                    _ -> return $ IRCall (T.pack name) convertedArgs

        Let name val -> do
            traceM $ "Converting let binding: " ++ name
            convertedVal <- convertExpr val
            state <- get
            case getExprType (varEnvironment state) convertedVal of
                Just valType -> do
                    modify $ \s -> s { varEnvironment = M.insert name valType (varEnvironment s) }
                    return $ IRLetAlloc (T.pack name) convertedVal IRAllocDefault
                Nothing -> throwError $ InvalidType "Cannot determine expression type for let binding"

        Var name -> do
            traceM $ "Converting variable reference: " ++ name
            state <- get
            case M.lookup name (varEnvironment state) of
                Nothing -> throwError $ UnknownVariable name
                Just t -> do
                    traceM $ "Found type for " ++ name ++ ": " ++ show t
                    return $ IRVar (T.pack name)

        _ -> do
            traceM $ "Encountered unsupported expression type: " ++ show expr
            throwError $ UnsupportedExpression "Unsupported expression type"

convertOp :: Op -> IROp
convertOp op = case op of
    Add -> IRAdd
    Sub -> IRSub
    Mul -> IRMul
    Div -> IRDiv
    Dot -> IRDot
    _ -> error "Unsupported operator"

convertType :: Type -> IRType
convertType (TypeNum nt) = IRTypeNum (convertNumType nt)
convertType (TypeVec vt) = IRTypeVec (convertVecType vt)
convertType TypeString = IRTypeString
convertType TypeBool = IRTypeBool
convertType (TypeStruct name fields) =
    IRTypeStruct (T.pack name) [(T.pack n, convertType t) | (n, t) <- fields]
convertType (TypeArray elemType) = IRTypeArray (convertType elemType)
convertType TypeVoid = IRTypeVoid
convertType TypeAny = IRTypeAny

convertNumType :: NumType -> IRNumType
convertNumType Int32 = IRInt32
convertNumType Int64 = IRInt64
convertNumType Float32 = IRFloat32
convertNumType Float64 = IRFloat64

convertVecType :: VecType -> IRVecType
convertVecType (Vec2 nt) = IRVec2 (convertNumType nt)
convertVecType (Vec3 nt) = IRVec3 (convertNumType nt)
convertVecType (Vec4 nt) = IRVec4 (convertNumType nt)

getExprType :: M.Map String IRType -> IRExpr -> Maybe IRType
getExprType env expr = case expr of
    IRNum t _ -> Just $ IRTypeNum t
    IRString _ -> Just IRTypeString
    IRBool _ -> Just IRTypeBool
    IRVec vt _ -> Just $ IRTypeVec vt
    IRVar name -> M.lookup (T.unpack name) env
    IRStructLit name fields -> Just $ IRTypeStruct name [(fname, fromMaybe IRTypeString (getExprType env fexpr)) | (fname, fexpr) <- fields]
    IRBinOp _ e1 e2 ->
        case (getExprType env e1, getExprType env e2) of
            (Just t1@(IRTypeVec _), Just t2@(IRTypeVec _)) | t1 == t2 -> Just t1
            (Just (IRTypeNum n1), Just (IRTypeNum n2)) | n1 == n2 -> Just (IRTypeNum n1)
            _ -> Nothing
    IRFieldAccess base field ->
        case getExprType env base of
            Just (IRTypeStruct _ fields) ->
                lookup field [(fn, ft) | (fn, ft) <- fields]
            _ -> Nothing
    _ -> Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x
