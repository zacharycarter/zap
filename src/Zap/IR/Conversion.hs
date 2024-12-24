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
import qualified Data.Set as S
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
  | TypeMismatch String
  deriving (Show, Eq)

type IRConverter = ExceptT IRConversionError (State IRGenState)

data IRGenState = IRGenState
  { tempCounter :: Int
  , blockCounter :: Int
  , varEnvironment :: M.Map String IRType
  , funcEnvironment :: M.Map String (IRType, [IRType])  -- Return type and param types
  }

convertToIR :: Program -> Either IRConversionError IR
convertToIR (Program tops) = runIRConverter (do
    traceM "\n=== Starting IR Conversion ==="

    -- Process type declarations first to build environment
    forM_ tops $ \case
        TLType name typ@(TypeStruct sName fields) -> do
            traceM $ "Adding struct type to environment: " ++ show sName
            let irFields = [(T.pack fn, convertType ft) | (fn, ft) <- fields]
            modify $ \s -> s { varEnvironment = M.insert sName (IRTypeStruct (T.pack sName) irFields) (varEnvironment s) }
        _ -> return ()

    -- Convert declarations
    decls <- mapM convertDecl [d | TLDecl d <- tops]
    let structDecls = [ IRStruct (T.pack name) [(T.pack fn, convertType ft) | (fn,ft) <- fields]
                      | TLType name (TypeStruct sName fields) <- tops, name == sName ]
    exprs <- concat <$> sequence [convertTopLevel t | t@(TLExpr _) <- tops]

    traceM "Finished converting all top-level expressions"
    return $ IRProgram (decls ++ structDecls) exprs) initState
  where
    initState = IRGenState
      { varEnvironment = M.empty
      , funcEnvironment = M.fromList
        [ ("print", (IRTypeVoid, [IRTypeAny]))  -- Built-in print function
        ]
      , tempCounter = 0
      , blockCounter = 0
      }

runIRConverter :: IRConverter a -> IRGenState -> Either IRConversionError a
runIRConverter m s = evalState (runExceptT m) s

convertTopLevel :: TopLevel -> IRConverter [IRExpr]
convertTopLevel (TLExpr expr) = do
    traceM $ "\nConverting top-level expression: " ++ show expr
    converted <- convertExpr expr
    return [converted]
convertTopLevel _ = return []

convertDecl :: Decl -> IRConverter IRDecl
convertDecl (DFunc name params retType body) = do
    traceM $ "Converting function declaration: " ++ name

    -- Save current environments
    originalVarEnv <- gets varEnvironment

    -- Add function to environment for recursive calls
    let paramTypes = [convertType t | Param _ t <- params]
    let irRetType = convertType retType
    modify $ \s -> s { funcEnvironment = M.insert name (irRetType, paramTypes) (funcEnvironment s) }

    -- Add parameters to var environment
    let paramVars = M.fromList [(name, convertType typ) | Param name typ <- params]
    modify $ \s -> s { varEnvironment = M.union paramVars (varEnvironment s) }

    -- Convert function body with parameters in scope
    convertedBody <- convertExpr body

    -- Restore original var environment
    modify $ \s -> s { varEnvironment = originalVarEnv }

    return $ IRFunc
        (T.pack name)
        [(T.pack n, convertType t) | Param n t <- params]
        (convertType retType)
        convertedBody

convertDecl (DStruct name fields) = do
    traceM $ "Converting struct declaration: " ++ name
    return $ IRStruct
        (T.pack name)
        [(T.pack fname, convertType ftype) | (fname, ftype) <- fields]

convertExpr :: Expr -> IRConverter IRExpr
convertExpr expr = do
    traceM $ "\n=== Converting Expression ==="
    traceM $ "Input expression: " ++ show expr

    let withMetadata node typ effects = do
          traceM $ "\nCreating metadata:"
          traceM $ "  Node: " ++ show node
          traceM $ "  Type: " ++ show typ
          traceM $ "  Effects: " ++ show effects
          return $ IRExpr
            { metadata = IRMetadata
              { exprType = typ
              , metaEffects = effects
              , metaSourcePos = Nothing
              }
            , expr = node
            }

    case expr of
        StrLit s -> do
            traceM "Converting string literal"
            withMetadata (IRString (T.pack s)) IRTypeString (S.singleton PureEffect)

        NumLit numType val -> do
            traceM $ "Converting numeric literal: " ++ val
            traceM $ "Numeric type: " ++ show numType
            let irType = IRTypeNum (convertNumType numType)
            withMetadata (IRNum (convertNumType numType) (T.pack val)) irType (S.singleton PureEffect)

        BoolLit b -> do
            traceM $ "Converting boolean literal: " ++ show b
            withMetadata (IRBool b) IRTypeBool (S.singleton PureEffect)

        Block scope -> do
            traceM $ "\nConverting block: " ++ blockLabel scope
            traceM $ "Block expressions count: " ++ show (length $ blockExprs scope)
            convertedExprs <- mapM convertExpr (blockExprs scope)
            forM_ convertedExprs $ \expr ->
                traceM $ "Block expression type: " ++ show (exprType $ metadata expr)

            convertedResult <- mapM convertExpr (blockResult scope)
            forM_ convertedResult $ \result ->
                traceM $ "Block result type: " ++ show (exprType $ metadata result)

            let blockType = case (convertedExprs, convertedResult) of
                    ([], Nothing) -> IRTypeNum IRInt32
                    (exprs, Nothing) -> exprType $ metadata $ last exprs
                    (_, Just result) -> exprType $ metadata result

            traceM $ "Final block type: " ++ show blockType
            withMetadata
                (IRBlockAlloc (T.pack $ blockLabel scope) convertedExprs convertedResult)
                blockType
                (inferBlockEffects convertedExprs convertedResult)

        Result expr -> do
            traceM "\nConverting result expression"
            converted <- convertExpr expr
            let resultType = exprType (metadata converted)
            traceM $ "Result expression type: " ++ show resultType
            withMetadata
                (IRResult converted)
                resultType
                (metaEffects $ metadata converted)

        If cond thenExpr elseExpr -> do
            traceM "\nConverting if expression"
            convertedCond <- convertExpr cond
            traceM $ "Condition type: " ++ show (exprType $ metadata convertedCond)
            convertedThen <- convertExpr thenExpr
            traceM $ "Then branch type: " ++ show (exprType $ metadata convertedThen)
            convertedElse <- convertExpr elseExpr
            traceM $ "Else branch type: " ++ show (exprType $ metadata convertedElse)

            let resultType = exprType $ metadata convertedThen
            let combinedEffects = S.unions [
                  metaEffects $ metadata convertedCond,
                  metaEffects $ metadata convertedThen,
                  metaEffects $ metadata convertedElse
                  ]
            let effects = if S.null (S.delete PureEffect combinedEffects)
                         then S.singleton PureEffect
                         else combinedEffects

            traceM $ "Selected if expression type: " ++ show resultType
            withMetadata
                (IRIf convertedCond convertedThen convertedElse)
                resultType
                effects

        BinOp op e1 e2 -> do
            traceM $ "\nConverting binary operation: " ++ show op
            converted1 <- convertExpr e1
            traceM $ "Left operand type: " ++ show (exprType $ metadata converted1)
            converted2 <- convertExpr e2
            traceM $ "Right operand type: " ++ show (exprType $ metadata converted2)
            resultType <- getBinOpType (convertOp op) converted1 converted2
            traceM $ "Binary operation result type: " ++ show resultType
            withMetadata
                (IRBinOp (convertOp op) converted1 converted2)
                resultType
                (S.singleton PureEffect)

        AssignOp name op expr -> do
          traceM $ "\nConverting binary assignment operation: " ++ show op
          val <- convertExpr expr
          traceM $ "Converted binary assignment operation expression: " ++ show val
          let varExpr = IRExpr
                { metadata = IRMetadata
                  { exprType = IRTypeNum IRInt32  -- We should infer this from context
                  , metaEffects = S.singleton ReadEffect
                  , metaSourcePos = Nothing
                  }
              , expr = IRVar (T.pack name)
              }
              binOp = IRBinOp (convertAssignOp op) varExpr val
              result = IRExpr
                { metadata = IRMetadata
                  { exprType = exprType (metadata val)
                  , metaEffects = S.fromList [ReadEffect, WriteEffect]
                  , metaSourcePos = Nothing
                  }
                , expr = binOp
                }

          return $ result

        Call "print" [arg] -> do
            traceM "\nConverting print call"
            convertedArg <- convertExpr arg
            traceM $ "Print argument type: " ++ show (exprType $ metadata convertedArg)
            withMetadata
                (IRPrint convertedArg)
                IRTypeVoid
                (S.singleton IOEffect)

        Call fname args -> do
            traceM $ "\nConverting function call: " ++ fname
            traceM $ "Argument count: " ++ show (length args)
            state <- get
            convertedArgs <- mapM convertExpr args
            forM_ (zip [1..] convertedArgs) $ \(i, arg) ->
                traceM $ "Argument " ++ show i ++ " type: " ++ show (exprType $ metadata arg)

            state <- get
            traceM $ "Looking up struct type for " ++ fname
            traceM $ "Current environment: " ++ show (varEnvironment state)

            case M.lookup fname (varEnvironment state) of
                Just structType ->
                    withMetadata
                        (IRCall (T.pack fname) convertedArgs)
                        structType
                        (S.singleton PureEffect)
                Nothing -> do
                    let (resultType, effects) = inferCallType fname convertedArgs
                    traceM $ "Inferred call type: " ++ show resultType
                    traceM $ "Inferred effects: " ++ show effects

                    case fname of
                        "Vec2" -> withMetadata (IRVec (IRVec2 IRFloat32) convertedArgs)
                                             (IRTypeVec (IRVec2 IRFloat32))
                                             (S.singleton PureEffect)
                        "Vec3" -> withMetadata (IRVec (IRVec3 IRFloat32) convertedArgs)
                                             (IRTypeVec (IRVec3 IRFloat32))
                                             (S.singleton PureEffect)
                        "Vec4" -> withMetadata (IRVec (IRVec4 IRFloat32) convertedArgs)
                                             (IRTypeVec (IRVec4 IRFloat32))
                                             (S.singleton PureEffect)
                        _ -> withMetadata (IRCall (T.pack fname) convertedArgs)
                                         resultType
                                         effects

        Let name val -> do
            traceM $ "\nConverting let binding: " ++ name
            convertedVal <- convertExpr val
            let valType = exprType $ metadata convertedVal
            traceM $ "Let binding value type: " ++ show valType

            modify $ \s -> s { varEnvironment = M.insert name valType (varEnvironment s) }
            withMetadata
                (IRLetAlloc (T.pack name) convertedVal IRAllocDefault)
                valType
                (S.insert WriteEffect $ metaEffects $ metadata convertedVal)

        Var name -> do
            traceM $ "\nConverting variable reference: " ++ name
            state <- get
            case M.lookup name (varEnvironment state) of
                Nothing -> do
                    traceM $ "ERROR: Unknown variable: " ++ name
                    throwError $ UnknownVariable name
                Just t -> do
                    traceM $ "Found variable type: " ++ show t
                    withMetadata
                        (IRVar (T.pack name))
                        t
                        (S.singleton ReadEffect)

        FieldAccess expr field -> do
            traceM $ "\nConverting field access expression: " ++ field
            convertedBase <- convertExpr expr
            traceM $ "Base expression type: " ++ show (exprType $ metadata convertedBase)
            case exprType (metadata convertedBase) of
                IRTypeStruct name fields -> do
                    traceM $ "Found struct type " ++ show name ++ " with fields: " ++ show fields
                    case lookup (T.pack field) fields of
                        Just fieldType -> do
                            traceM $ "Found field type: " ++ show fieldType
                            withMetadata
                                (IRFieldAccess convertedBase (T.pack field))
                                fieldType
                                (S.singleton ReadEffect)
                        Nothing -> throwError $ InvalidType $ "Field not found: " ++ field
                other -> do
                    traceM $ "Not a struct type: " ++ show other
                    throwError $ InvalidType "Field access on non-struct type"

        VarDecl name val -> do
            traceM $ "Converting variable declaration: " ++ name
            convertedVal <- convertExpr val
            let valType = exprType $ metadata convertedVal
            -- Add variable to environment
            modify $ \s -> s { varEnvironment = M.insert name valType (varEnvironment s) }
            -- Variable declarations use let allocation with default strategy
            return $ mkPureExpr valType $ IRLetAlloc (T.pack name) convertedVal IRAllocDefault

        While cond body -> do
            traceM $ "Converting while expression"
            -- Convert condition
            convertedCond <- convertExpr cond
            traceM $ "Converted condition: " ++ show convertedCond

            -- Convert body
            convertedBody <- convertExpr body
            traceM $ "Converted body: " ++ show convertedBody

            return $ mkEffectfulExpr IRTypeVoid (S.fromList [ReadEffect, WriteEffect]) $
                IRBlock (T.pack "while") [convertedCond, convertedBody] Nothing

        _ -> do
            traceM $ "\nERROR: Unsupported expression: " ++ show expr
            throwError $ UnsupportedExpression "Unsupported expression type"

validateBinaryOp :: Op -> IRExpr -> IRExpr -> IRConverter ()
validateBinaryOp op e1 e2 = do
    let t1 = exprType $ metadata e1
    let t2 = exprType $ metadata e2
    case op of
        Add -> validateArithmetic t1 t2
        Sub -> validateArithmetic t1 t2
        Mul -> validateArithmetic t1 t2
        Div -> validateArithmetic t1 t2
        Dot -> validateDot t1 t2
        _ -> return ()
  where
    validateArithmetic :: IRType -> IRType -> IRConverter ()
    validateArithmetic t1 t2 = do
        case (t1, t2) of
            (IRTypeNum n1, IRTypeNum n2) | n1 == n2 -> return ()
            (IRTypeVec v1, IRTypeVec v2) | v1 == v2 -> return ()
            (IRTypeNum _, IRTypeNum _) -> throwError $ InvalidType "Mismatched numeric types"
            _ -> throwError $ InvalidType "Invalid operands for arithmetic operation"

    validateDot :: IRType -> IRType -> IRConverter ()
    validateDot t1 t2 = do
        case (t1, t2) of
            (IRTypeVec v1, IRTypeVec v2) | v1 == v2 -> return ()
            _ -> throwError $ InvalidType "Dot product requires matching vector types"

convertOp :: Op -> IROp
convertOp op = case op of
    Add -> IRAdd
    Sub -> IRSub
    Mul -> IRMul
    Div -> IRDiv
    Dot -> IRDot
    Lt -> IRLt
    Gt -> IRGt
    Eq -> IREq
    _ -> error "Unsupported operator"

convertAssignOp :: Op -> IROp
convertAssignOp op = case op of
    Add -> IRAddAssign
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

-- Helper functions for type and effect inference
getBinOpType :: IROp -> IRExpr -> IRExpr -> IRConverter IRType
getBinOpType op e1 e2 = case (op, exprType $ metadata e1, exprType $ metadata e2) of
    (IRAdd, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
        return $ IRTypeNum t1
    (IRSub, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
        return $ IRTypeNum t1
    (IRMul, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
        return $ IRTypeNum t1
    (IRDiv, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
        return $ IRTypeNum t1
    (IRAdd, IRTypeVec v1, IRTypeVec v2) | v1 == v2 ->
        return $ IRTypeVec v1
    (IRDot, IRTypeVec v1, IRTypeVec v2) | v1 == v2 ->
        return $ case v1 of
            IRVec2 t -> IRTypeNum t
            IRVec3 t -> IRTypeNum t
            IRVec4 t -> IRTypeNum t
    (IRLt, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
        return IRTypeBool
    (IRGt, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
        return IRTypeBool
    (IREq, IRTypeNum t1, IRTypeNum t2) | t1 == t2 ->
        return IRTypeBool
    _ -> throwError $ InvalidType $ "Type mismatch in binary operation: " ++
         show (exprType $ metadata e1) ++ " and " ++
         show (exprType $ metadata e2)

mergeNumericTypes :: IRType -> IRType -> IRType
mergeNumericTypes (IRTypeNum t1) (IRTypeNum t2) | t1 == t2 = IRTypeNum t1
mergeNumericTypes (IRTypeVec v1) (IRTypeVec v2) | v1 == v2 = IRTypeVec v1
mergeNumericTypes _ _ = IRTypeAny

inferCallType :: String -> [IRExpr] -> (IRType, S.Set Effect)
inferCallType name args = case name of
    "Vec2" -> (IRTypeVec (IRVec2 IRFloat32), S.singleton PureEffect)
    "Vec3" -> (IRTypeVec (IRVec3 IRFloat32), S.singleton PureEffect)
    "Vec4" -> (IRTypeVec (IRVec4 IRFloat32), S.singleton PureEffect)
    _ -> (IRTypeAny, S.unions $ map (metaEffects . metadata) args)

inferBlockEffects :: [IRExpr] -> Maybe IRExpr -> S.Set Effect
inferBlockEffects exprs mResult =
    let exprEffects = map (metaEffects . metadata) exprs
        resultEffects = maybe S.empty (metaEffects . metadata) mResult
    in S.unions (resultEffects : exprEffects)

blockResultType :: Maybe IRExpr -> IRType
blockResultType Nothing = IRTypeVoid
blockResultType (Just expr) = exprType $ metadata expr
