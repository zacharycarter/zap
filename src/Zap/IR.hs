{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zap.IR
  ( IRProgram (..),
    IRLiteral (..),
    IRFuncDecl (..),
    IRBlock (..),
    IRStmt (..),
    IRExpr (..),
    IRType (..),
    IRMetadata (..),
    Effect (..),
    IRConversionError (..),
    convertFuncDecl,
    convertToIR',
    convertToIRExpr,
    convertToIRExprWithSymbols,
    LiteralType (..),
    TypeError (..),
    TypeVar (..),
  )
where

import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Except
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace
import Zap.AST as A
import Zap.Analysis.Semantic (isFnameStructConstructor)

--------------------------------------------------------------------------------
--                           IR Data Structures
--------------------------------------------------------------------------------

data LiteralType
  = LitInt NumType
  | LitFloat NumType
  | LitString
  | LitBoolean
  deriving (Show, Eq)

data IRMetadata = IRMetadata
  { metaType :: IRType,
    metaEffects :: S.Set Effect,
    metaSourcePos :: Maybe (Int, Int),
    metaLiteralType :: Maybe LiteralType,
    metaSymTable :: Maybe SymbolTable
  }
  deriving (Show, Eq)

data Effect
  = ReadEffect
  | WriteEffect
  | IOEffect
  | PureEffect
  deriving (Show, Eq, Ord)

data IRProgram = IRProgram
  { irFuncs :: [(IRFuncDecl, IRMetadata)]
  }
  deriving (Show, Eq)

data IRFuncDecl = IRFuncDecl
  { fnName :: String,
    fnParams :: [(String, IRType)],
    fnRetType :: IRType,
    fnBody :: IRBlock
  }
  deriving (Show, Eq)

data IRBlock = IRBlock
  { irBlockLabel :: String,
    irBlockStmts :: [(IRStmt, IRMetadata)]
  }
  deriving (Show, Eq)

data IRStmt
  = IRStmtExpr IRExpr
  | IRReturn (Maybe IRExpr)
  | IRVarDecl
      { varDeclName :: String,
        varDeclType :: IRType,
        varDeclInit :: IRExpr
      }
  | IRAssign String IRExpr
  | IRAssignOp String Op IRExpr
  | IRLabel String
  | IRGoto String
  | IRJumpIfTrue IRExpr String
  | IRJumpIfZero IRExpr String
  | IRProcCall String [IRExpr]
  deriving (Show, Eq)

data IRExpr
  = IRCall String [IRExpr]
  | IRVar String
  | IRLit IRLiteral
  deriving (Show, Eq)

data IRLiteral
  = IRBoolLit Bool
  | IRStringLit String
  | IRInt32Lit Int
  | IRInt64Lit Int
  | IRFloat32Lit Float
  | IRFloat64Lit Double
  | IRVarRef String
  deriving (Show, Eq)

data IRType
  = IRTypeVoid
  | IRTypeInt32
  | IRTypeInt64
  | IRTypeFloat32
  | IRTypeFloat64
  | IRTypeString
  | IRTypeVar TypeVar
  | IRTypeFunc [IRType] IRType
  | IRTypeStruct String StructId
  deriving (Show, Eq)

data NumericPrecision = P32 | P64
  deriving (Eq, Ord)

data IRConversionError
  = IRError String
  | IRTypeError IRType IRType
  | IRUnsupportedExpr String
  | IRUnsupportedLiteral String
  | IRInvalidFunction String
  | IRMissingMain
  | IRInvalidStructInitialization String
  deriving (Show, Eq)

data TypeInstance = TypeInstance
  { baseType :: String,
    typeArgs :: [IRType]
  }
  deriving (Show, Eq)

newtype TypeVar = TypeVar Int
  deriving (Eq, Ord, Show)

data TypeConstraint
  = TEq IRType IRType
  | TVar TypeVar IRType
  | TFunc TypeVar [IRType] IRType
  deriving (Eq, Show)

type TypeSubst = M.Map TypeVar IRType

data TypeError
  = UnificationError IRType IRType
  | InfiniteType TypeVar IRType
  | UnboundVariable T.Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------
--                          Metadata Helpers
--------------------------------------------------------------------------------

mkMetadata :: IRType -> S.Set Effect -> IRMetadata
mkMetadata typ effs =
  IRMetadata
    { metaType = typ,
      metaEffects = effs,
      metaSourcePos = Nothing,
      metaLiteralType = Nothing,
      metaSymTable = Nothing
    }

mkLiteralMetadata :: IRType -> S.Set Effect -> LiteralType -> IRMetadata
mkLiteralMetadata typ effs litType =
  IRMetadata
    { metaType = typ,
      metaEffects = effs,
      metaSourcePos = Nothing,
      metaLiteralType = Just litType,
      metaSymTable = Nothing
    }

data LoopContext = LoopContext
  { loopNumber :: Int,
    loopEndLabel :: String
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
--                           Program Conversion
--------------------------------------------------------------------------------

convertToIR' :: Program -> SymbolTable -> Either IRConversionError IRProgram
convertToIR' (Program tops) symTable = do
  traceM "\n=== Converting Program to IR ==="
  traceM $ "Initial symbol table: " ++ show symTable

  let (structDefs, restTops) = partitionStructs tops
  traceM $ "Found struct definitions: " ++ show structDefs

  let (funcs, exprs) = partitionFuncs restTops
  traceM $ "Found functions: " ++ show funcs
  traceM $ "Found expressions: " ++ show exprs

  convertedFuncs <- mapM (convertFuncDecl symTable) funcs
  (mainBlock, mainMeta) <- convertTops symTable exprs Nothing
  let mainFunc =
        ( IRFuncDecl
            { fnName = "main",
              fnParams = [],
              fnRetType = IRTypeVoid,
              fnBody = mainBlock
            },
          -- Here's where we need to include the final symbol table
          mainMeta {metaSymTable = Just symTable}
        )

  return $ IRProgram (convertedFuncs ++ [mainFunc])
  where
    partitionStructs :: [TopLevel] -> ([(String, Type)], [TopLevel])
    partitionStructs = foldr splitStruct ([], [])
      where
        splitStruct (TLType name t@(TypeStruct _ _)) (ss, ts) =
          ((name, t) : ss, ts)
        splitStruct t (ss, ts) = (ss, t : ts)

    partitionFuncs :: [TopLevel] -> ([Decl], [TopLevel])
    partitionFuncs = foldr splitFunc ([], [])
      where
        splitFunc (TLDecl d@(DFunc _ _ _ _ _)) (fs, ts) =
          (d : fs, ts)
        splitFunc t (fs, ts) = (fs, t : ts)

--------------------------------------------------------------------------------
--                    Convert Function Declarations
--------------------------------------------------------------------------------

convertStructType :: Type -> Either IRConversionError IRType
convertStructType (TypeStruct sid name) =
  Right $ IRTypeStruct name sid
convertStructType t = Right $ convertType t

-- | Check if last statement is IRReturn
endsInReturn :: [(IRStmt, IRMetadata)] -> Bool
endsInReturn [] = False
endsInReturn stmts =
  case fst (last stmts) of
    IRReturn _ -> True
    _ -> False

convertFuncDecl ::
  SymbolTable ->
  Decl ->
  Either IRConversionError (IRFuncDecl, IRMetadata)
convertFuncDecl symTable (DFunc name typeParams params retType (Block label bodyExprs blockResult)) = do
  traceM $ "\n=== Converting function: " ++ name
  traceM $ "Parameters: " ++ show params
  traceM $ "Return type: " ++ show retType
  traceM $ "Converting params: " ++ show params
  traceM $ "Type params: " ++ show typeParams

  let irParams =
        map
          ( \(Param pname ptyp) ->
              case ptyp of
                -- For specialized struct constructors, get type from field
                TypeNum t -> do
                  traceM $ "Converting param " ++ pname ++ " type: " ++ show t
                  (pname, convertType ptyp)
                TypeStruct _ _ -> (pname, convertType ptyp)
                -- Avoid converting type parameters to void
                TypeParam p -> do
                  traceM $ "Converting param " ++ show p ++ " with type: " ++ show ptyp
                  case M.lookup name (structNames symTable) of
                    Just sid -> case lookupStruct sid symTable of
                      Just def -> case structFields def of
                        [("value", fieldType)] -> (pname, convertType fieldType)
                        _ -> (pname, convertType ptyp)
                      Nothing -> (pname, convertType ptyp)
                    Nothing -> (pname, convertType ptyp)
                _ -> (pname, convertType ptyp)
          )
          params

  traceM $ "Converted params: " ++ show irParams
  let retTypeIR = convertType retType
  traceM $ "Converted return type: " ++ show retTypeIR

  -- Rest of the existing conversion logic
  traceM $ "Converting body expressions: " ++ show bodyExprs
  convertedStmts <- concat <$> mapM (convertExprToStmts symTable Nothing) bodyExprs
  traceM $ "converted statements: " ++ show convertedStmts

  let alreadyEndsInReturn =
        case reverse convertedStmts of
          ((IRReturn _, _) : _) -> True
          _ -> False

  traceM $ "alreadyEndsInReturn" ++ show alreadyEndsInReturn

  let returnMeta = mkMetadata retTypeIR (S.singleton PureEffect)
  let finalStmts =
        if alreadyEndsInReturn
          then convertedStmts
          else convertedStmts ++ [(IRReturn Nothing, returnMeta)]

  traceM $ "Final statements: " ++ show finalStmts

  let bodyBlock = IRBlock "function.entry" finalStmts
  let funcMeta = mkMetadata retTypeIR (S.singleton PureEffect)

  pure
    ( IRFuncDecl
        { fnName = name,
          fnParams = irParams,
          fnRetType = retTypeIR,
          fnBody = bodyBlock
        },
      funcMeta
    )
convertFuncDecl _ (DFunc name _ _ _ body) =
  Left $
    IRUnsupportedExpr $
      "Function body must be a block: " ++ show body

--------------------------------------------------------------------------------
--                     Convert Expressions to IR Stmts
--------------------------------------------------------------------------------

convertExprToStmts ::
  SymbolTable ->
  Maybe LoopContext ->
  Expr ->
  Either IRConversionError [(IRStmt, IRMetadata)]
convertExprToStmts symTable ctx expr = do
  traceM $ "=== Converting expression to statements: " ++ show expr
  traceM $ "Expression: " ++ show expr
  case expr of
    If cond thenExpr elseExpr -> do
      traceM "Converting if/else expression to statements"
      traceM $ "Cond: " ++ show cond
      traceM $ "Then expr: " ++ show thenExpr
      traceM $ "Else expr: " ++ show elseExpr
      -- We delegate to our new function
      convertIfFullyReturning symTable ctx cond thenExpr elseExpr
    Let name val -> do
      traceM $ "Converting Let binding for: " ++ name
      when (isStructCall val) $ validateStructTypes val symTable
      convertedExpr <- convertToIRExprWithSymbols symTable val

      let declaredType = lookupVarType name symTable
      let exprType = typeFromExpr val (Just symTable)

      traceM $ "Expression type: " ++ show exprType

      case (declaredType, exprType) of
        (Just (TypeNum Int32), IRTypeInt64) ->
          Left $ IRTypeError IRTypeInt32 IRTypeInt64
        _ -> do
          let irType = maybe exprType convertType declaredType
          traceM $ "Meta type: " ++ show irType

          traceM $ "Creating metadata for: " ++ show val
          let meta = case val of
                Lit (IntLit _ mtype) ->
                  let litType = case mtype of
                        Just nt -> LitInt nt
                        Nothing -> LitInt Int32
                   in mkLiteralMetadata irType (S.singleton WriteEffect) litType
                Lit (FloatLit _ mtype) ->
                  let litType = case mtype of
                        Just nt -> LitFloat nt
                        Nothing -> LitFloat Float32
                   in mkLiteralMetadata irType (S.singleton WriteEffect) litType
                Lit (StringLit _) ->
                  mkLiteralMetadata IRTypeString (S.singleton WriteEffect) LitString
                StructLit _ _ ->
                  mkMetadata irType (S.singleton WriteEffect)
                _ ->
                  mkMetadata irType (S.singleton WriteEffect)

          traceM $ "Created metadata: " ++ show meta
          traceM $ "Assigned type: " ++ show irType

          let stmt = IRVarDecl name irType convertedExpr
          traceM $ "Created statement: " ++ show stmt
          return [(stmt, meta)]
    BinOp op e1 e2 -> do
      traceM $ "Converting binary operation: " ++ show op
      left <- convertToIRExprWithSymbols symTable e1
      right <- convertToIRExprWithSymbols symTable e2
      let opStr = case op of
            Add -> "Add"
            Sub -> "Sub"
            Mul -> "Mul"
            Div -> "Div"
            _ -> error $ "Unsupported operator: " ++ show op
      let stmt = IRStmtExpr (IRCall opStr [left, right])
      return [(stmt, mkMetadata IRTypeInt32 (S.singleton PureEffect))]
    Call "print" [arg] -> do
      traceM $ "=== Converting print statement ==="
      traceM $ "Input arg: " ++ show arg
      converted <- convertToIRExprWithSymbols symTable arg
      traceM $ "Converted arg: " ++ show converted
      let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
      let stmt = (IRProcCall "print" [converted], meta)
      traceM $ "Generated statement: " ++ show stmt
      return [stmt]
    Var name -> do
      -- When converting a variable reference in function position, make it a return
      traceM $ "Converting variable reference to return: " ++ name
      let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
      return [(IRReturn (Just (IRVar name)), meta)]
    Break (Just label) mexpr -> do
      traceM $ "\n=== convertExprToStmts: Break ==="
      traceM $ "Label: " ++ show label
      traceM $ "Value: " ++ show mexpr
      traceM $ "Context: " ++ show ctx
      case mexpr of
        Just expr -> do
          traceM $ "Break has value expression: " ++ show expr
          convertedExpr <- convertToIRExprWithSymbols symTable expr
          let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
          traceM $ "Converted break value to IR: " ++ show convertedExpr
          return [(IRReturn (Just convertedExpr), meta)]
        Nothing -> do
          traceM $ "Break has no value expression"
          let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
          return [(IRGoto label, meta)]
    Break Nothing Nothing -> do
      traceM $ "\n=== convertExprToStmts: Unlabeled Break ==="
      case ctx of
        Just loopCtx -> do
          traceM $ "Current loop context: " ++ show loopCtx
          let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
          return [(IRGoto (loopEndLabel loopCtx), meta)]
        _ -> Left $ IRError "Invalid break syntax"
    _ -> do
      traceM $ "No pattern matched in convertExprToStmts, falling through to: " ++ show expr
      converted <- convertToIRExprWithSymbols symTable expr
      let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
      return [(IRStmtExpr converted, meta)]

--------------------------------------------------------------------------------
-- Convert If with fully-returning branches
--------------------------------------------------------------------------------

-- | If both then/else blocks end in IRReturn, skip the final label
convertIfFullyReturning ::
  SymbolTable ->
  Maybe LoopContext ->
  Expr -> -- condition
  Expr -> -- thenExpr
  Expr -> -- elseExpr
  Either IRConversionError [(IRStmt, IRMetadata)]
convertIfFullyReturning symTable ctx cond thenExpr elseExpr = do
  traceM $ "\n=== convertIfFullyReturning ==="
  traceM $ "Converting if/else expression"
  traceM $ "Condition: " ++ show cond
  traceM $ "Then expr: " ++ show thenExpr
  traceM $ "Else expr: " ++ show elseExpr

  condExpr <- convertToIRExprWithSymbols symTable cond
  traceM $ "Converted condition: " ++ show condExpr

  thenStmts <- convertBlockOrLiteralReturn symTable thenExpr ctx
  traceM $ "Converted then statements: " ++ show thenStmts

  elseStmts <- convertBlockOrLiteralReturn symTable elseExpr ctx
  traceM $ "Converted else statements: " ++ show elseStmts

  let metaVoid = mkMetadata IRTypeVoid (S.singleton PureEffect)
  let metaInt = mkMetadata IRTypeInt32 (S.singleton PureEffect)

  -- Look for break with value in then branch
  let (breakValue, remainingThen) = splitAtFunctionBreak thenStmts
  traceM $ "\n=== Break Analysis ==="
  traceM $ "Analyzing statements for function breaks: " ++ show thenStmts
  traceM $ "Found break value: " ++ show breakValue
  traceM $ "Remaining then statements: " ++ show remainingThen

  case (ctx, breakValue) of
    -- Loop break case
    (Just loopCtx, Nothing)
      | any
          ( \(stmt, _) ->
              case stmt of
                IRGoto label -> label == loopEndLabel loopCtx
                _ -> False
          )
          thenStmts -> do
          traceM "\n=== Converting Loop Break ==="
          let loopStartLabel = "while_" ++ show (loopNumber loopCtx) ++ "_start"
          let jumpIfZero = (IRJumpIfZero condExpr loopStartLabel, metaVoid)
          return $ jumpIfZero : thenStmts

    -- Function return break case
    (_, Just returnValue) -> do
      traceM $ "\n=== Converting Break to Return ==="
      let jumpIfZero = (IRJumpIfZero condExpr "if_else", metaVoid)
      let labelElse = (IRLabel "if_else", metaVoid)
      let thenReturn = [(IRReturn (Just returnValue), metaInt)]
      elseReturn <- case remainingThen of
        ((IRStmtExpr expr, _) : _) -> return [(IRReturn (Just expr), metaInt)]
        _ -> return [(IRReturn Nothing, metaInt)]
      return $ jumpIfZero : thenReturn ++ [labelElse] ++ elseReturn

    -- Optimize if both branches are simple expressions
    _ | all isSimpleStmt thenStmts && all isSimpleStmt elseStmts -> do
      traceM "Simple expression branches - optimizing control flow"
      let jumpIfZero = (IRJumpIfZero condExpr "if_else", metaVoid)
      let labelElse = (IRLabel "if_else", metaVoid)
      return $ jumpIfZero : thenStmts ++ [labelElse] ++ elseStmts

    -- Optimize if both then and else statements end in return
    _ | endsInReturn thenStmts && endsInReturn elseStmts -> do
      traceM "Both branches end in return - optimizing control flow"
      let jumpIfZero = (IRJumpIfZero condExpr "if_else", metaVoid)
      let labelElse = (IRLabel "if_else", metaVoid)
      return $ jumpIfZero : thenStmts ++ [labelElse] ++ elseStmts

    -- Fallback case for complex if/else
    _ -> do
      traceM $ "\n=== Regular If/Else Control Flow ==="
      let endLabel = "if_end"
      let jumpIfZero = (IRJumpIfZero condExpr "if_else", metaVoid)
      let gotoEnd = (IRGoto endLabel, metaVoid)
      let labelElse = (IRLabel "if_else", metaVoid)
      let labelEnd = (IRLabel endLabel, metaVoid)
      return $ jumpIfZero : thenStmts ++ [gotoEnd, labelElse] ++ elseStmts ++ [labelEnd]
  where
    isSimpleStmt :: (IRStmt, IRMetadata) -> Bool
    isSimpleStmt (IRStmtExpr (IRLit _), _) = True
    isSimpleStmt (IRStmtExpr (IRVar _), _) = True
    isSimpleStmt _ = False

-- Helper to find function break and its value - pure function
splitAtFunctionBreak :: [(IRStmt, IRMetadata)] -> (Maybe IRExpr, [(IRStmt, IRMetadata)])
splitAtFunctionBreak stmts = do
  let (before, rest) = break isFuncBreak stmts
  case rest of
    ((IRGoto label, _) : next : remaining)
      | isFunctionLabel label -> case next of
          (IRStmtExpr val, _) -> (Just val, before ++ remaining)
          _ -> (Nothing, before ++ remaining)
    _ -> (Nothing, stmts)
  where
    isFuncBreak (IRGoto label, _) = isFunctionLabel label
    isFuncBreak _ = False

-- Helper to identify function breaks - pure function
isFunctionBreak :: IRStmt -> Bool
isFunctionBreak (IRGoto label) = isFunctionLabel label
isFunctionBreak _ = False

--------------------------------------------------------------------------------
--             Single-Expression Block => IRReturn
--------------------------------------------------------------------------------

convertBlockOrLiteralReturn ::
  SymbolTable ->
  Expr ->
  Maybe LoopContext ->
  Either IRConversionError [(IRStmt, IRMetadata)]
convertBlockOrLiteralReturn symTable (Block _ [e] Nothing) ctx = do
  traceM "Single-expression block => IRReturn"
  converted <- convertToIRExprWithSymbols symTable e
  let metaType = typeFromExpr e (Just symTable)
  let meta = mkMetadata metaType (S.singleton PureEffect)
  pure [(IRReturn (Just converted), meta)]
convertBlockOrLiteralReturn symTable other ctx =
  convertBlock symTable other ctx

--------------------------------------------------------------------------------
--                  Validate Struct Types (unchanged)
--------------------------------------------------------------------------------

validateStructTypes :: Expr -> SymbolTable -> Either IRConversionError ()
validateStructTypes (Call fname [Lit lit]) _
  | "_i32" `T.isSuffixOf` (T.pack fname) = case lit of
      IntLit _ (Just Int32) -> Right ()
      _ ->
        Left $
          IRTypeError
            IRTypeInt32
            (convertType $ literalToType lit)
  | "_i64" `T.isSuffixOf` (T.pack fname) = case lit of
      IntLit _ (Just Int64) -> Right ()
      _ ->
        Left $
          IRTypeError
            IRTypeInt64
            (convertType $ literalToType lit)
  | "_f32" `T.isSuffixOf` (T.pack fname) = case lit of
      FloatLit _ (Just Float32) -> Right ()
      _ ->
        Left $
          IRTypeError
            IRTypeFloat32
            (convertType $ literalToType lit)
  | "_f64" `T.isSuffixOf` (T.pack fname) = case lit of
      FloatLit _ (Just Float64) -> Right ()
      _ ->
        Left $
          IRTypeError
            IRTypeFloat64
            (convertType $ literalToType lit)
validateStructTypes (FieldAccess baseExpr fieldName) symTable = do
  case baseExpr of
    Var varName ->
      case lookupVarType varName symTable of
        Just (TypeStruct sid _) ->
          case lookupStruct sid symTable of
            Just def ->
              case lookup fieldName (structFields def) of
                Just fieldType -> Right ()
                Nothing ->
                  Left $
                    IRError $
                      "Field " ++ fieldName ++ " not found"
            Nothing -> Left $ IRError "Invalid struct type"
        _ -> Left $ IRError "Not a struct type"
    _ -> Left $ IRError "Invalid base expression for field access"
validateStructTypes _ _ = Right ()

literalToType :: Literal -> Type
literalToType (IntLit _ (Just t)) = TypeNum t
literalToType (FloatLit _ (Just t)) = TypeNum t
literalToType _ = TypeVoid

isStructCall :: Expr -> Bool
isStructCall (Call fname _) = isFnameStructConstructor fname
isStructCall _ = False

--------------------------------------------------------------------------------
--                Type Inference Helpers (unchanged)
--------------------------------------------------------------------------------

typeFromExpr :: Expr -> Maybe SymbolTable -> IRType
typeFromExpr (Lit (FloatLit _ (Just Float32))) _ = IRTypeFloat32
typeFromExpr (Lit (FloatLit _ (Just Float64))) _ = IRTypeFloat64
typeFromExpr (Lit (IntLit _ (Just Int32))) _ = IRTypeInt32
typeFromExpr (Lit (IntLit _ (Just Int64))) _ = IRTypeInt64
typeFromExpr (Call fname args) symTable
  | isFnameStructConstructor fname =
      let _ =
            trace
              ( "\n=== typeFromExpr for struct call ==="
                  ++ "\nLooking up: "
                  ++ fname
                  ++ "\nSymbol table structs: "
                  ++ show (structNames <$> symTable)
              )
              ()
       in case symTable >>= \st -> M.lookup fname (structNames st) of
            Just sid -> IRTypeStruct fname sid
            Nothing ->
              -- Check if this is a specialized name (e.g. Box_i32)
              case symTable >>= \st -> M.lookup fname (structNames st) of
                Just sid -> IRTypeStruct fname sid
                Nothing -> error $ "Unknown struct type: " ++ fname
typeFromExpr (Call fname _) _
  | isFnameStructConstructor fname =
      IRTypeStruct fname (StructId 0)
typeFromExpr (StructLit name _) _ =
  IRTypeStruct name (StructId 0)
typeFromExpr (FieldAccess baseExpr fieldName) (Just st) =
  case baseExpr of
    Var varName ->
      case lookupVarType varName st of
        Just (TypeStruct sid structName) ->
          case lookupStruct sid st of
            Just def ->
              case lookup fieldName (structFields def) of
                Just fieldType -> convertType fieldType
                Nothing -> IRTypeVoid
            Nothing -> IRTypeVoid
        _ -> IRTypeVoid
    _ -> IRTypeVoid
typeFromExpr _ _ = IRTypeVoid

--------------------------------------------------------------------------------
--         Convert Type from AST to IR (unchanged)
--------------------------------------------------------------------------------
convertType :: Type -> IRType
convertType t =
  let _ =
        trace
          ( "\n=== convertType ===\n"
              ++ "Converting type: "
              ++ show t
          )
          ()
      result = case t of
        TypeNum Int32 -> IRTypeInt32
        TypeNum Int64 -> IRTypeInt64
        TypeNum Float32 -> IRTypeFloat32
        TypeNum Float64 -> IRTypeFloat64
        TypeVoid -> IRTypeVoid
        TypeStruct sid nm -> IRTypeStruct nm sid
        TypeParam p -> IRTypeVar (TypeVar 0)
        _ -> IRTypeVoid
      _ = trace ("Converted to: " ++ show result) ()
   in result

--------------------------------------------------------------------------------
--           Convert Toplevels
--------------------------------------------------------------------------------
convertTops ::
  SymbolTable ->
  [TopLevel] ->
  Maybe LoopContext ->
  Either IRConversionError (IRBlock, IRMetadata)
convertTops symTable tops ctx = do
  traceM "\n=== convertTops: START ==="
  traceM $ "Current ctx: " ++ show ctx
  traceM $ "Tops levels before conversion: " ++ show tops
  stmts <- concat <$> mapM (\t -> convertTop symTable t ctx) tops
  traceM $ "Full converted statements: " ++ show stmts

  let lastMeta =
        if null stmts
          then mkMetadata IRTypeVoid (S.singleton PureEffect)
          else snd (last stmts)

  traceM $ "Last meta: " ++ show lastMeta

  let alreadyEndsInReturn =
        case reverse stmts of
          ((IRReturn _, _) : _) -> True
          _ -> False

  traceM $ "alreadyEndsInReturn" ++ show alreadyEndsInReturn

  let returnStmt = (IRReturn Nothing, mkMetadata IRTypeVoid (S.singleton PureEffect))
  let finalStmts = stmts ++ [returnStmt | not alreadyEndsInReturn]

  traceM $ "Final statement list: " ++ show finalStmts
  traceM "=== convertTops: END ==="

  -- Add symbol table to final metadata
  let finalMeta = lastMeta {metaSymTable = Just symTable}
  traceM $ "Final metadata with symbol table: " ++ show finalMeta

  return (IRBlock "main.entry" finalStmts, finalMeta)

--------------------------------------------------------------------------------
--           Convert a Single Toplevel Expression
--------------------------------------------------------------------------------

convertTop symTable (TLExpr e@(If cond thenExpr elseExpr)) ctx = do
  traceM $ "\n=== convertTop: If expression ==="
  traceM $ "Condition: " ++ show cond
  traceM $ "Context: " ++ show ctx
  traceM $ "Then expr: " ++ show thenExpr
  traceM $ "Else expr: " ++ show elseExpr

  condExpr <- convertToIRExprWithSymbols symTable cond

  case (thenExpr, ctx) of
    -- Break case
    (Block _ [Break Nothing Nothing] Nothing, Just loopCtx) -> do
      let loopStartLabel = "while_" ++ show (loopNumber loopCtx) ++ "_start"
      let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
      return $
        [ (IRJumpIfZero (IRCall "Not" [condExpr]) loopStartLabel, meta),
          (IRGoto (loopEndLabel loopCtx), meta)
        ]

    -- Case for simple block expressions
    _ -> do
      thenStmts <- convertBlock symTable thenExpr ctx
      elseStmts <- convertBlock symTable elseExpr ctx

      let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
      let elseLabel = "if_else"
      let endLabel = "if_end"

      return $
        (IRJumpIfZero condExpr elseLabel, meta)
          : thenStmts
          ++ [ (IRGoto endLabel, meta),
               (IRLabel elseLabel, meta)
             ]
          ++ elseStmts
          ++ [(IRLabel endLabel, meta)]
  where
    isSimpleBlock (Block _ [Lit _] Nothing) = True
    isSimpleBlock _ = False
convertTop symTable (TLExpr (While cond body)) prevCtx = do
  traceM "\n=== Converting While Loop ==="
  let nextNum = case prevCtx of
        Just c -> loopNumber c + 1
        Nothing -> 0
  let startLabel = "while_" ++ show nextNum ++ "_start"
  let endLabel = "while_" ++ show nextNum ++ "_end"
  let ctx = LoopContext nextNum endLabel
  traceM $ "Labels: start=" ++ startLabel ++ " end=" ++ endLabel

  condExpr <- convertToIRExprWithSymbols symTable cond
  traceM $ "Condition: " ++ show condExpr

  bodyStmts <- convertBlock symTable body (Just ctx)
  traceM $ "Raw body statements: " ++ show bodyStmts

  let cleanBody = removeTrailingGoto startLabel bodyStmts
  traceM $ "Clean body before appending: " ++ show cleanBody

  let endsWithBreak = case cleanBody of
        [] -> False
        stmts -> case fst (last stmts) of
          IRGoto label -> "_end" `T.isSuffixOf` T.pack label
          _ -> False

  traceM $ "Body ends with break: " ++ show endsWithBreak

  let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
  let result =
        [ (IRLabel startLabel, meta),
          (IRJumpIfZero condExpr endLabel, meta)
        ]
          ++ cleanBody
          ++
          -- Only add back-edge if we don't break
          ( if not endsWithBreak
              then [(IRGoto startLabel, meta)]
              else []
          )
          ++ [ (IRLabel endLabel, meta)
             ]

  traceM $ "Final IR with conditional back-edge: " ++ show result
  pure result
  where
    removeTrailingGoto :: String -> [(IRStmt, IRMetadata)] -> [(IRStmt, IRMetadata)]
    removeTrailingGoto startLabel stmts =
      -- Find the first break statement (goto to end label)
      let (beforeBreak, afterBreak) = break isLoopBreak stmts
       in case afterBreak of
            -- Keep statements before break and the break itself only
            (breakStmt@(IRGoto label, _) : rest)
              | isEndLabel label ->
                  beforeBreak ++ [breakStmt] -- Drop everything after break
                  -- No break found - keep trailing goto to start
            [] -> stmts
      where
        isLoopBreak (IRGoto label, _) = isEndLabel label
        isLoopBreak _ = False

        isEndLabel label = "_end" `T.isSuffixOf` T.pack label
convertTop symTable (TLExpr (VarDecl name value)) ctx = do
  -- Unchanged from your code
  traceM $ "Converting top-level variable declaration: " ++ name
  convertedExpr <- convertToIRExprWithSymbols symTable value
  let irType =
        case value of
          Lit (IntLit _ mtype) ->
            case mtype of
              Just Int32 -> IRTypeInt32
              Just Int64 -> IRTypeInt64
              Nothing -> IRTypeInt32
          Lit (FloatLit _ mtype) ->
            case mtype of
              Just Float32 -> IRTypeFloat32
              Just Float64 -> IRTypeFloat64
              Nothing -> IRTypeFloat32
          Lit (StringLit _) -> IRTypeString
          _ -> IRTypeVoid
  let meta =
        case value of
          Lit (IntLit _ mtype) ->
            let litType =
                  case mtype of
                    Just nt -> LitInt nt
                    Nothing -> LitInt Int32
             in mkLiteralMetadata irType (S.singleton WriteEffect) litType
          Lit (FloatLit _ mtype) ->
            let litType =
                  case mtype of
                    Just nt -> LitFloat nt
                    Nothing -> LitFloat Float32
             in mkLiteralMetadata irType (S.singleton WriteEffect) litType
          Lit (StringLit _) ->
            mkLiteralMetadata IRTypeString (S.singleton WriteEffect) LitString
          _ -> mkMetadata IRTypeVoid (S.singleton WriteEffect)
  pure [(IRVarDecl name irType convertedExpr, meta)]
convertTop symTable (TLExpr (Break label mexpr)) ctx = case label of
  -- Handle labeled break - treat as return from the labeled block
  Just lbl -> do
    case mexpr of
      Just expr -> do
        convertedExpr <- convertToIRExprWithSymbols symTable expr
        let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
        return [(IRReturn (Just convertedExpr), meta)]
      Nothing ->
        return [(IRGoto lbl, mkMetadata IRTypeVoid (S.singleton PureEffect))]

  -- Handle unlabeled break - must be in a loop context
  Nothing -> case ctx of
    Just loopCtx -> do
      let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
      pure [(IRGoto (loopEndLabel loopCtx), meta)]
    Nothing -> Left $ IRError "Unlabeled break must be inside a loop"
convertTop symTable (TLExpr (Assign name expr)) _ = do
  convertedExpr <- convertToIRExprWithSymbols symTable expr
  let meta = mkMetadata IRTypeVoid (S.singleton WriteEffect)
  pure [(IRAssign name convertedExpr, meta)]
convertTop symTable (TLExpr (AssignOp name op expr)) _ = do
  convertedExpr <- convertToIRExprWithSymbols symTable expr
  let meta = mkMetadata IRTypeVoid (S.singleton WriteEffect)
  pure [(IRAssignOp name op convertedExpr, meta)]
convertTop symTable (TLExpr e) ctx =
  case e of
    Let {} -> convertExprToStmts symTable ctx e
    _ -> (: []) <$> convertExpr symTable e

--------------------------------------------------------------------------------
--            convertBlock (unchanged except minor details)
--------------------------------------------------------------------------------

convertBlock ::
  SymbolTable ->
  Expr ->
  Maybe LoopContext ->
  Either IRConversionError [(IRStmt, IRMetadata)]
convertBlock symTable (Block _ blockExprs _) ctx = do
  stmtsLists <- mapM (\e -> convertTop symTable (TLExpr e) ctx) blockExprs
  pure $ concat stmtsLists
convertBlock symTable expr _ = do
  single <- convertExpr symTable expr
  pure [single]

--------------------------------------------------------------------------------
--             convertExpr logic (unchanged except for If case)
--------------------------------------------------------------------------------

convertExpr ::
  SymbolTable ->
  Expr ->
  Either IRConversionError (IRStmt, IRMetadata)
convertExpr symTable (Call "print" [arg]) = do
  -- Unchanged printing logic
  traceM $ "Converting print expression with arg: " ++ show arg
  case arg of
    BinOp op e1 e2 | op `elem` [Add, Sub, Mul, Div] -> do
      left <- convertToLiteral e1
      right <- convertToLiteral e2
      result <- evalBinOp op left right
      let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
      pure (IRProcCall "print" [IRLit result], meta)
    Call fname args -> do
      convArgs <- mapM (convertToIRExprWithSymbols symTable) args
      let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
      pure (IRProcCall "print" [IRCall fname convArgs], meta)
    FieldAccess b f -> do
      conv <- convertToIRExprWithSymbols symTable (FieldAccess b f)
      let meta = mkMetadata IRTypeFloat32 (S.singleton IOEffect)
      pure (IRProcCall "print" [conv], meta)
    Var n -> do
      let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
      pure (IRProcCall "print" [IRVar n], meta)
    _ -> do
      conv <- convertToIRExprWithSymbols symTable arg
      let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
      pure (IRProcCall "print" [conv], meta)
convertExpr symTable (Lit lit) = do
  irExpr <- convertToIRExprWithSymbols symTable (Lit lit)
  let meta = case lit of
        IntLit _ mtype ->
          let l = case mtype of
                Just nt -> LitInt nt
                Nothing -> LitInt Int64
           in mkLiteralMetadata IRTypeInt64 (S.singleton PureEffect) l
        FloatLit _ mtype ->
          let f = case mtype of
                Just nt -> LitFloat nt
                Nothing -> LitFloat Float32
           in mkLiteralMetadata IRTypeFloat32 (S.singleton PureEffect) f
        StringLit _ ->
          mkLiteralMetadata IRTypeString (S.singleton PureEffect) LitString
        BooleanLit _ ->
          mkLiteralMetadata IRTypeInt32 (S.singleton PureEffect) LitBoolean
  pure (IRStmtExpr irExpr, meta)
convertExpr symTable e@(If _ _ _) = do
  traceM "Skipping If in convertExpr—already handled by statement-based logic"
  let dummy = IRStmtExpr (IRLit (IRInt32Lit 0))
  let dMeta = mkMetadata IRTypeVoid (S.singleton PureEffect)
  pure (dummy, dMeta)
convertExpr symTable (BinOp op e1 e2) = do
  irExpr <- convertToIRExprWithSymbols symTable (BinOp op e1 e2)
  pure (IRStmtExpr irExpr, mkMetadata IRTypeInt32 (S.singleton PureEffect))
convertExpr _ (Call "print" _) =
  Left $ IRInvalidFunction "print requires exactly one argument"
convertExpr _ e = do
  traceM $ "Encoutnered unsupported expression inside convertExpr: " ++ show e
  Left $ IRUnsupportedExpr $ "Unsupported expression: " ++ show e

--------------------------------------------------------------------------------
--             Evaluate Binary Ops at Compile Time
--------------------------------------------------------------------------------

evalBinOp :: Op -> IRLiteral -> IRLiteral -> Either IRConversionError IRLiteral
evalBinOp Add (IRInt32Lit x) (IRInt32Lit y) = Right $ IRInt32Lit (x + y)
evalBinOp Add (IRInt64Lit x) (IRInt64Lit y) = Right $ IRInt64Lit (x + y)
evalBinOp Add (IRFloat32Lit x) (IRFloat32Lit y) = Right $ IRFloat32Lit (x + y)
evalBinOp Add (IRFloat64Lit x) (IRFloat64Lit y) = Right $ IRFloat64Lit (x + y)
evalBinOp Sub (IRInt32Lit x) (IRInt32Lit y) = Right $ IRInt32Lit (x - y)
evalBinOp Sub (IRInt64Lit x) (IRInt64Lit y) = Right $ IRInt64Lit (x - y)
evalBinOp Sub (IRInt64Lit x) (IRInt32Lit y) = Right $ IRInt64Lit (x - fromIntegral y)
evalBinOp Sub (IRInt32Lit x) (IRInt64Lit y) = Right $ IRInt64Lit (fromIntegral x - y)
evalBinOp Sub (IRFloat32Lit x) (IRFloat32Lit y) = Right $ IRFloat32Lit (x - y)
evalBinOp Sub (IRFloat64Lit x) (IRFloat64Lit y) = Right $ IRFloat64Lit (x - y)
evalBinOp Mul (IRInt32Lit x) (IRInt32Lit y) = Right $ IRInt32Lit (x * y)
evalBinOp Mul (IRInt64Lit x) (IRInt64Lit y) = Right $ IRInt64Lit (x * y)
evalBinOp Div (IRInt32Lit x) (IRInt32Lit y)
  | y == 0 = Left $ IRUnsupportedLiteral "Division by zero"
  | otherwise = Right $ IRInt32Lit (x `div` y)
evalBinOp Div (IRInt64Lit x) (IRInt64Lit y)
  | y == 0 = Left $ IRUnsupportedLiteral "Division by zero"
  | otherwise = Right $ IRInt64Lit (x `div` y)
evalBinOp Lt (IRInt32Lit x) (IRInt32Lit y) =
  Right $ IRInt32Lit (if x < y then 1 else 0)
evalBinOp Lt (IRInt64Lit x) (IRInt64Lit y) =
  Right $ IRInt64Lit (if x < y then 1 else 0)
evalBinOp Gt (IRInt32Lit x) (IRInt32Lit y) =
  Right $ IRInt32Lit (if x > y then 1 else 0)
evalBinOp Gt (IRInt64Lit x) (IRInt64Lit y) =
  Right $ IRInt64Lit (if x > y then 1 else 0)
evalBinOp Eq (IRInt32Lit x) (IRInt32Lit y) =
  Right $ IRInt32Lit (if x == y then 1 else 0)
evalBinOp Eq (IRInt64Lit x) (IRInt64Lit y) =
  Right $ IRInt64Lit (if x == y then 1 else 0)
evalBinOp op _ _ =
  Left $ IRUnsupportedLiteral $ "Unsupported operator: " ++ show op

--------------------------------------------------------------------------------
--      Convert Expression to Literal  (unchanged)
--------------------------------------------------------------------------------

convertToLiteral :: Expr -> Either IRConversionError IRLiteral
convertToLiteral expr = case expr of
  Lit lit -> case lit of
    IntLit val mtype ->
      case mtype of
        Just Int32 -> Right $ IRInt32Lit (read val)
        Just Int64 -> Right $ IRInt64Lit (read val)
        Nothing ->
          if read val > (2 ^ 31 - 1)
            then Right $ IRInt64Lit (read val)
            else Right $ IRInt32Lit (read val)
    FloatLit val mtype ->
      case mtype of
        Just Float32 -> Right $ IRFloat32Lit (read val)
        Just Float64 -> Right $ IRFloat64Lit (read val)
        Nothing -> Right $ IRFloat32Lit (read val)
    StringLit s -> Right $ IRStringLit s
    BooleanLit _ ->
      Left $ IRUnsupportedLiteral "Boolean literals not yet supported"
  Var name -> Right $ IRVarRef name
  BinOp op e1 e2 -> do
    left <- convertToLiteral e1
    right <- convertToLiteral e2
    evalBinOp op left right
  _ ->
    Left $
      IRUnsupportedLiteral $
        "Unsupported literal: " ++ show expr

--------------------------------------------------------------------------------
--      Convert Expression to IR (unchanged except for If skip)
--------------------------------------------------------------------------------

convertToIRExpr :: Expr -> Either IRConversionError IRExpr
convertToIRExpr expr = convertToIRExprWithSymbols emptySymbolTable expr

convertToIRExprWithSymbols ::
  SymbolTable ->
  Expr ->
  Either IRConversionError IRExpr
convertToIRExprWithSymbols symTable (VarDecl name value) = do
  traceM $ "Converting variable declaration in expression: " ++ name
  convertedExpr <- convertToIRExprWithSymbols symTable value
  Right $ IRCall "var_decl" [IRLit (IRStringLit name), convertedExpr]
convertToIRExprWithSymbols _ (Lit lit) = case lit of
  IntLit val mtype ->
    case mtype of
      Just Int32 -> Right $ IRLit $ IRInt32Lit (read val)
      Just Int64 -> Right $ IRLit $ IRInt64Lit (read val)
      Nothing ->
        if read val > (2 ^ 31 - 1)
          then Right $ IRLit $ IRInt64Lit (read val)
          else Right $ IRLit $ IRInt32Lit (read val)
  FloatLit val mtype ->
    case mtype of
      Just Float32 -> Right $ IRLit $ IRFloat32Lit (read val)
      Just Float64 -> Right $ IRLit $ IRFloat64Lit (read val)
      Nothing -> Right $ IRLit $ IRFloat32Lit (read val)
  StringLit val -> Right $ IRLit $ IRStringLit val
  BooleanLit val -> Right $ IRLit $ IRBoolLit val
convertToIRExprWithSymbols _ (Var name) =
  Right $ IRVar name
convertToIRExprWithSymbols symTable (Call fname args)
  | isFnameStructConstructor fname = do
      traceM $ "\n=== convertToIRExprWithSymbols: struct constructor ==="
      traceM $ "Constructor name: " ++ fname
      traceM $ "Arguments: " ++ show args
      convertedArgs <- mapM (convertToIRExprWithSymbols symTable) args
      Right $
        IRCall
          "struct_lit"
          (IRLit (IRStringLit fname) : convertedArgs)
  | otherwise = do
      traceM $ "\n=== convertToIRExprWithSymbols Call ==="
      traceM $ "Function name: " ++ fname
      traceM $ "Args: " ++ show args
      convertedArgs <- mapM (convertToIRExprWithSymbols symTable) args
      traceM $ "Converted args: " ++ show convertedArgs
      Right $ IRCall fname convertedArgs
convertToIRExprWithSymbols symTable (Block _ blockExprs _) = do
  traceM $ "\n=== convertToIRExprWithSymbols: Block ==="
  case blockExprs of
    [] -> Left $ IRError "Empty block"
    exprs -> convertToIRExprWithSymbols symTable (last exprs)
convertToIRExprWithSymbols symTable (BinOp op e1 e2) = do
  traceM $ "\n=== convertToIRExprWithSymbols: BinOp ==="
  left <- convertToIRExprWithSymbols symTable e1
  right <- convertToIRExprWithSymbols symTable e2
  let result = case op of
        Lt -> IRCall "Lt" [left, right]
        Gt -> IRCall "Gt" [left, right]
        Eq -> IRCall "Eq" [left, right]
        _ -> IRCall (opToString op) [left, right]
  Right result
convertToIRExprWithSymbols symTable (FieldAccess expr field) = do
  traceM $ "\n=== convertToIRExprWithSymbols: FieldAccess ==="
  convBase <- convertToIRExprWithSymbols symTable expr
  let result = IRCall "field_access" [convBase, IRLit (IRStringLit field)]
  Right result
convertToIRExprWithSymbols symTable (StructLit name fields) = do
  traceM $ "\n=== convertToIRExprWithSymbols: StructLit ==="
  convFields <-
    mapM
      ( \(f, e) -> do
          i <- convertToIRExprWithSymbols symTable e
          pure (f, i)
      )
      fields
  Right $
    IRCall
      "struct_lit"
      ( IRLit (IRStringLit name)
          : concatMap (\(f, i) -> [IRLit (IRStringLit f), i]) convFields
      )
convertToIRExprWithSymbols _ (If _ _ _) = do
  traceM "Skipping If expression in convertToIRExprWithSymbols - already handled"
  Right $ IRLit (IRInt32Lit 0)
convertToIRExprWithSymbols _ e = do
  traceM $
    "Encoutnered unsupported expression inside convertToIRExprWithSymbols: "
      ++ show e
  Left $ IRUnsupportedExpr $ "Unsupported expression: " ++ show e

--------------------------------------------------------------------------------
-- Helpers for struct detection, operator conversion, etc.
--------------------------------------------------------------------------------
opToString :: Op -> String
opToString Add = "Add"
opToString Sub = "Sub"
opToString Mul = "Mul"
opToString Div = "Div"
opToString Lt = "Lt"
opToString Gt = "Gt"
opToString Eq = "Eq"
opToString NotEq = "NotEq"
opToString op = error $ "Unsupported operator: " ++ show op
