{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Zap.IR
  ( IRProgram(..)
  , IRLiteral(..)
  , IRFuncDecl(..)
  , IRBlock(..)
  , IRStmt(..)
  , IRExpr(..)
  , IRType(..)
  , IRMetadata(..)
  , Effect(..)
  , IRConversionError(..)
  , convertFuncDecl
  , convertToIR'
  , convertToIRExpr
  , convertToIRExprWithSymbols
  , LiteralType(..)
  , TypeError(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , TypeSubst
  , generateConstraints
  , solveConstraints
  , applySubst
  ) where

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.Except
import Debug.Trace

import Zap.AST as A

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
  { metaType :: IRType
  , metaEffects :: S.Set Effect
  , metaSourcePos :: Maybe (Int, Int)
  , metaLiteralType :: Maybe LiteralType
  , metaSymTable :: Maybe SymbolTable
  } deriving (Show, Eq)

data Effect
  = ReadEffect
  | WriteEffect
  | IOEffect
  | PureEffect
  deriving (Show, Eq, Ord)

data IRProgram = IRProgram
  { irFuncs :: [(IRFuncDecl, IRMetadata)]
  } deriving (Show, Eq)

data IRFuncDecl = IRFuncDecl
  { fnName :: String
  , fnParams :: [(String, IRType)]
  , fnRetType :: IRType
  , fnBody :: IRBlock
  } deriving (Show, Eq)

data IRBlock = IRBlock
  { irBlockLabel :: String
  , irBlockStmts :: [(IRStmt, IRMetadata)]
  } deriving (Show, Eq)

data IRStmt
  = IRStmtExpr IRExpr
  | IRReturn (Maybe IRExpr)
  | IRVarDecl
      { varDeclName :: String
      , varDeclType :: IRType
      , varDeclInit :: IRExpr
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
  { baseType :: String
  , typeArgs :: [IRType]
  } deriving (Show, Eq)

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
mkMetadata typ effs = IRMetadata
  { metaType = typ
  , metaEffects = effs
  , metaSourcePos = Nothing
  , metaLiteralType = Nothing
  , metaSymTable = Nothing
  }

mkLiteralMetadata :: IRType -> S.Set Effect -> LiteralType -> IRMetadata
mkLiteralMetadata typ effs litType = IRMetadata
  { metaType = typ
  , metaEffects = effs
  , metaSourcePos = Nothing
  , metaLiteralType = Just litType
  , metaSymTable = Nothing
  }

data LoopContext = LoopContext
  { loopNumber :: Int
  , loopEndLabel :: String
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
--                           Program Conversion
--------------------------------------------------------------------------------

convertToIR' :: Program -> SymbolTable -> Either IRConversionError IRProgram
convertToIR' (Program tops) symTable = do
    traceM "\n=== Converting Program to IR ==="

    let (structDefs, restTops) = partitionStructs tops
    traceM $ "Found struct definitions: " ++ show structDefs

    let (funcs, exprs) = partitionFuncs restTops
    traceM $ "Found functions: " ++ show funcs
    traceM $ "Found expressions: " ++ show exprs

    -- Handle specialized function generation
    let specializedFuncs = concatMap (generateSpecializedVersions symTable) funcs
    traceM $ "Generated specialized functions: " ++ show specializedFuncs

    convertedFuncs <- mapM (convertFuncDecl symTable) (funcs ++ specializedFuncs)
    (mainBlock, mainMeta) <- convertTops symTable exprs Nothing
    let mainFunc = ( IRFuncDecl
          { fnName = "main"
          , fnParams = []
          , fnRetType = IRTypeVoid
          , fnBody = mainBlock
          }
          , mainMeta { metaSymTable = Just symTable }
          )

    return $ IRProgram (convertedFuncs ++ [mainFunc])
  where
    partitionStructs :: [TopLevel] -> ([(String, Type)], [TopLevel])
    partitionStructs = foldr splitStruct ([], [])
      where
        splitStruct (TLType name t@(TypeStruct _ _)) (ss, ts) =
            ((name, t) : ss, ts)
        splitStruct t (ss, ts) = (ss, t:ts)

    partitionFuncs :: [TopLevel] -> ([Decl], [TopLevel])
    partitionFuncs = foldr splitFunc ([], [])
      where
        splitFunc (TLDecl d@(DFunc _ _ _ _ _)) (fs, ts) =
            (d:fs, ts)
        splitFunc t (fs, ts) = (fs, t:ts)

-- Helper for function specialization
generateSpecializedVersions :: SymbolTable -> Decl -> [Decl]
generateSpecializedVersions st (DFunc name typeParams params retType body) = do
    traceM $ "\n=== Generationg specialized version of " ++ name ++ " ==="
    -- Look for specialized versions in symbol table using String operations
    let specialized = M.filterWithKey
          (\k _ -> (name ++ "_") `isPrefixOf` k)
          (funcDefs st)

    traceM $ "Found specialized versions of " ++ name ++ ": " ++ show specialized

    M.elems $ M.mapWithKey (\specName def ->
        -- Create specialized declaration with concrete types
        let concreteType = case dropWhile (/= '_') specName of
              "_i32" -> TypeNum Int32
              "_i64" -> TypeNum Int64
              "_f32" -> TypeNum Float32
              "_f64" -> TypeNum Float64
              _ -> TypeNum Int32  -- Default fallback
        in DFunc specName []
             [Param n (substituteTypeParamWithSymbols tp concreteType t st) |
              Param n t <- params,
              tp <- typeParams]
             (substituteTypeParamWithSymbols (head typeParams) concreteType retType st)
             body
      ) specialized
  where
    -- String-specific prefix check
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

generateSpecializedVersions _ _ = []

--------------------------------------------------------------------------------
--                    Convert Function Declarations
--------------------------------------------------------------------------------

convertStructType :: Type -> Either IRConversionError IRType
convertStructType (TypeStruct sid name) =
    Right $ IRTypeStruct name sid
convertStructType t = Right $ convertType t

-- | NEW: Check if last statement is IRReturn
endsInReturn :: [(IRStmt, IRMetadata)] -> Bool
endsInReturn [] = False
endsInReturn stmts =
  case fst (last stmts) of
    IRReturn _ -> True
    _          -> False
-- END NEW

convertFuncDecl
  :: SymbolTable
  -> Decl
  -> Either IRConversionError (IRFuncDecl, IRMetadata)
convertFuncDecl symTable (DFunc name typeParams params retType (Block label bodyExprs blockResult)) = do
    traceM $ "\n=== Converting function: " ++ name
    traceM $ "Parameters: " ++ show params
    traceM $ "Return type: " ++ show retType
    traceM $ "Converting params: " ++ show params
    traceM $ "Type params: " ++ show typeParams

    -- NEW: Check if this is a specialized version
    let isSpecialized = '_' `elem` name
    let baseType = if isSpecialized
                    then let suffix = dropWhile (/= '_') name
                         in case (drop 1 suffix) of
                              "i32" -> Just IRTypeInt32
                              "i64" -> Just IRTypeInt64
                              "f32" -> Just IRTypeFloat32
                              "f64" -> Just IRTypeFloat64
                              _ -> Nothing
                    else Nothing

    traceM $ "Is specialized: " ++ show isSpecialized
    traceM $ "Base type: " ++ show baseType

    -- Modify param conversion to handle specialization
    let irParams = case baseType of
          Just concreteType ->
            [(pname, concreteType) | Param pname _ <- params]
          Nothing ->
            [(pname, convertType ptyp) | Param pname ptyp <- params]
    traceM $ "IRParams: " ++ show irParams

    -- Rest of the existing conversion logic
    traceM $ "Converting body expressions: " ++ show bodyExprs
    convertedStmts <- concat <$> mapM (convertExprToStmts symTable Nothing) bodyExprs
    traceM $ "converted statements: " ++ show convertedStmts

    let alreadyEndsInReturn =
          case reverse convertedStmts of
            ((IRReturn _, _):_) -> True
            _                   -> False

    traceM $ "alreadyEndsInReturn" ++ show alreadyEndsInReturn

    -- Modify return type based on specialization
    let retTypeIR = case baseType of
          Just concreteType -> concreteType
          Nothing -> convertType retType
    let returnMeta = mkMetadata retTypeIR (S.singleton PureEffect)

    -- Keep existing return statement handling
    let finalStmts =
          if alreadyEndsInReturn
            then convertedStmts
            else convertedStmts ++ [(IRReturn Nothing, returnMeta)]

    traceM $ "Final statements: " ++ show finalStmts

    let bodyBlock = IRBlock "function.entry" finalStmts
    let funcMeta  = mkMetadata retTypeIR (S.singleton PureEffect)

    pure ( IRFuncDecl
             { fnName     = name
             , fnParams   = irParams
             , fnRetType  = retTypeIR
             , fnBody     = bodyBlock
             }
         , funcMeta
         )

convertFuncDecl _ (DFunc name _ _ _ body) =
  Left $ IRUnsupportedExpr $
    "Function body must be a block: " ++ show body

--------------------------------------------------------------------------------
--                     Convert Expressions to IR Stmts
--------------------------------------------------------------------------------

convertExprToStmts
  :: SymbolTable
  -> Maybe LoopContext
  -> Expr
  -> Either IRConversionError [(IRStmt, IRMetadata)]
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
        let exprType     = typeFromExpr val (Just symTable)

        traceM $ "Expression type after specialization: " ++ show exprType

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
          left  <- convertToIRExprWithSymbols symTable e1
          right <- convertToIRExprWithSymbols symTable e2
          let opStr = case op of
                Add -> "Add"
                Sub -> "Sub"
                Mul -> "Mul"
                Div -> "Div"
                _   -> error $ "Unsupported operator: " ++ show op
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
              return [ (IRReturn (Just convertedExpr), meta) ]
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
-- NEW FUNCTION: Convert If with fully-returning branches
--------------------------------------------------------------------------------

-- | If both then/else blocks end in IRReturn, skip the final label
convertIfFullyReturning
  :: SymbolTable
  -> Maybe LoopContext
  -> Expr    -- condition
  -> Expr    -- thenExpr
  -> Expr    -- elseExpr
  -> Either IRConversionError [(IRStmt, IRMetadata)]
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
        -- Keep existing loop break case
        (Just loopCtx, Nothing) | any (\(stmt, _) ->
          case stmt of
            IRGoto label -> label == loopEndLabel loopCtx
            _ -> False) thenStmts -> do
            traceM "\n=== Converting Loop Break ==="
            let loopStartLabel = "while_" ++ show (loopNumber loopCtx) ++ "_start"
            let jumpIfZero = (IRJumpIfZero condExpr loopStartLabel, metaVoid)
            return $ jumpIfZero : thenStmts

        -- Keep existing function return break case unchanged
        (_, Just returnValue) -> do
            traceM $ "\n=== Converting Break to Return ==="
            let jumpIfZero = (IRJumpIfZero condExpr "if_else", metaVoid)
            let labelElse = (IRLabel "if_else", metaVoid)
            let thenReturn = [(IRReturn (Just returnValue), metaInt)]
            elseReturn <- case remainingThen of
                ((IRStmtExpr expr, _):_) -> return [(IRReturn (Just expr), metaInt)]
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

        -- Keep existing fallback case for complex if/else
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
        ((IRGoto label, _):next:remaining)
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


-- Helper to check if a label is a function label
isFunctionLabel :: String -> Bool
isFunctionLabel label =
    -- A break target is always the function name
    not $ any (`T.isPrefixOf` (T.pack label)) ["if_", "while_", "else_", "end"]

--------------------------------------------------------------------------------
--             Single-Expression Block => IRReturn
--------------------------------------------------------------------------------

convertBlockOrLiteralReturn
  :: SymbolTable
  -> Expr
  -> Maybe LoopContext
  -> Either IRConversionError [(IRStmt, IRMetadata)]
convertBlockOrLiteralReturn symTable (Block _ [e] Nothing) ctx = do
    traceM "Single-expression block => IRReturn"
    converted <- convertToIRExprWithSymbols symTable e
    let metaType = typeFromExpr e (Just symTable)
    let meta     = mkMetadata metaType (S.singleton PureEffect)
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
      _ -> Left $ IRTypeError IRTypeInt32
                         (convertType $ literalToType lit)
  | "_i64" `T.isSuffixOf` (T.pack fname) = case lit of
      IntLit _ (Just Int64) -> Right ()
      _ -> Left $ IRTypeError IRTypeInt64
                         (convertType $ literalToType lit)
  | "_f32" `T.isSuffixOf` (T.pack fname) = case lit of
      FloatLit _ (Just Float32) -> Right ()
      _ -> Left $ IRTypeError IRTypeFloat32
                         (convertType $ literalToType lit)
  | "_f64" `T.isSuffixOf` (T.pack fname) = case lit of
      FloatLit _ (Just Float64) -> Right ()
      _ -> Left $ IRTypeError IRTypeFloat64
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
                Nothing -> Left $ IRError $
                    "Field " ++ fieldName ++ " not found"
            Nothing -> Left $ IRError "Invalid struct type"
        _ -> Left $ IRError "Not a struct type"
    _ -> Left $ IRError "Invalid base expression for field access"
validateStructTypes _ _ = Right ()

literalToType :: Literal -> Type
literalToType (IntLit _ (Just t))   = TypeNum t
literalToType (FloatLit _ (Just t)) = TypeNum t
literalToType _                     = TypeVoid

isStructCall :: Expr -> Bool
isStructCall (Call fname _) = isFnameStructConstructor fname
isStructCall _              = False

--------------------------------------------------------------------------------
--                Type Inference Helpers (unchanged)
--------------------------------------------------------------------------------

typeFromExpr :: Expr -> Maybe SymbolTable -> IRType
typeFromExpr (Lit (FloatLit _ (Just Float32))) _ = IRTypeFloat32
typeFromExpr (Lit (FloatLit _ (Just Float64))) _ = IRTypeFloat64
typeFromExpr (Lit (IntLit _ (Just Int32)))     _ = IRTypeInt32
typeFromExpr (Lit (IntLit _ (Just Int64)))     _ = IRTypeInt64
typeFromExpr (Call fname args) symTable
  | isFnameStructConstructor fname =
      if '[' `elem` fname && ']' `elem` fname then
        let baseName = takeWhile (/= '[') fname
            paramType =
              case dropWhile (/= '[') fname of
                ('[':'i':'3':'2':']':_) -> TypeNum Int32
                ('[':'i':'6':'4':']':_) -> TypeNum Int64
                ('[':'f':'3':'2':']':_) -> TypeNum Float32
                ('[':'f':'6':'4':']':_) -> TypeNum Float64
                _ -> error $ "Invalid type parameter in: " ++ fname
        in case symTable >>= \st -> M.lookup baseName (structNames st) of
              Just sid -> IRTypeStruct (getSpecializedName baseName paramType) sid
              Nothing  -> error $ "Unknown base struct type: " ++ baseName
      else
        case symTable >>= \st -> M.lookup fname (structNames st) of
          Just sid ->
            case args of
              [Lit (IntLit _ (Just Int32))] ->
                IRTypeStruct (fname ++ "_i32") sid
              _ ->
                IRTypeStruct fname sid
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
                  Nothing        -> IRTypeVoid
              Nothing -> IRTypeVoid
          _ -> IRTypeVoid
      _ -> IRTypeVoid
typeFromExpr _ _ = IRTypeVoid

--------------------------------------------------------------------------------
--         Convert Type from AST to IR (unchanged)
--------------------------------------------------------------------------------

convertType :: Type -> IRType
convertType (TypeNum Int32)     = IRTypeInt32
convertType (TypeNum Int64)     = IRTypeInt64
convertType TypeVoid            = IRTypeVoid
convertType (TypeStruct sid nm) = IRTypeStruct nm sid
convertType (TypeParam _)       = IRTypeVar (TypeVar 0)
convertType _                   = IRTypeVoid

--------------------------------------------------------------------------------
--           Convert Toplevels
--------------------------------------------------------------------------------
convertTops :: SymbolTable -> [TopLevel] -> Maybe LoopContext
           -> Either IRConversionError (IRBlock, IRMetadata)
convertTops symTable tops ctx = do
    traceM "\n=== convertTops: START ==="
    traceM $ "Current ctx: " ++ show ctx
    traceM $ "Tops levels before conversion: " ++ show tops
    stmts <- concat <$> mapM (\t -> convertTop symTable t ctx) tops
    traceM $ "Full converted statements: " ++ show stmts

    let lastMeta = if null stmts
          then mkMetadata IRTypeVoid (S.singleton PureEffect)
          else snd (last stmts)

    traceM $ "Last meta: " ++ show lastMeta
    -- traceM $ "Examining last statement for flow: " ++ case stmts of
    --     [] -> "NO STATEMENTS"
    --     _ -> show (last stmts)

    -- let hasTerminatingFlow = any isFlowTerminator stmts
    -- traceM $ "Statements ending in flow terminator: " ++ show (filter isFlowTerminator stmts)
    -- traceM $ "Individual flow termination checks:"
    -- forM_ stmts $ \stmt ->
    --     traceM $ "  " ++ show (fst stmt) ++ " -> " ++ show (isFlowTerminator stmt)

    -- traceM $ "hasTerminatingFlow= " ++ show hasTerminatingFlow

    let alreadyEndsInReturn =
          case reverse stmts of
            ((IRReturn _, _):_) -> True
            _                   -> False

    traceM $ "alreadyEndsInReturn" ++ show alreadyEndsInReturn

    let returnStmt = (IRReturn Nothing, mkMetadata IRTypeVoid (S.singleton PureEffect))
    let finalStmts = stmts ++ [returnStmt | not alreadyEndsInReturn]

    traceM $ "Final statement list: " ++ show finalStmts
    traceM "=== convertTops: END ==="

    return (IRBlock "main.entry" finalStmts, lastMeta)
  -- where
  --   isFlowTerminator :: (IRStmt, IRMetadata) -> Bool
  --   isFlowTerminator (stmt, meta) = case stmt of
  --       IRReturn _ -> True
  --       -- IRGoto label | "_end" `T.isSuffixOf` T.pack label -> True
  --       -- Simple expression statements in if/else blocks are complete
  --       IRStmtExpr expr -> case expr of
  --           IRLit _ -> True  -- Literal values indicate completed control flow
  --           _ -> False
  --       _ -> False

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
        -- Keep existing break case unchanged
        (Block _ [Break Nothing Nothing] Nothing, Just loopCtx) -> do
            let loopStartLabel = "while_" ++ show (loopNumber loopCtx) ++ "_start"
            let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
            return $ [ (IRJumpIfZero (IRCall "Not" [condExpr]) loopStartLabel, meta)
                    , (IRGoto (loopEndLabel loopCtx), meta)
                    ]

        -- Add new case for simple block expressions
        _ -> do
            thenStmts <- convertBlock symTable thenExpr ctx
            elseStmts <- convertBlock symTable elseExpr ctx

            let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
            let elseLabel = "if_else"
            let endLabel = "if_end"

            return $ (IRJumpIfZero condExpr elseLabel, meta)
                    : thenStmts
                    ++ [ (IRGoto endLabel, meta)
                       , (IRLabel elseLabel, meta)
                       ]
                    ++ elseStmts
                    ++ [ (IRLabel endLabel, meta) ]

  where
    isSimpleBlock (Block _ [Lit _] Nothing) = True
    isSimpleBlock _ = False

convertTop symTable (TLExpr (While cond body)) prevCtx = do
    traceM "\n=== Converting While Loop ==="
    let nextNum = case prevCtx of
          Just c -> loopNumber c + 1
          Nothing -> 0
    let startLabel = "while_" ++ show nextNum ++ "_start"
    let endLabel   = "while_" ++ show nextNum ++ "_end"
    let ctx        = LoopContext nextNum endLabel
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
    let result = [ (IRLabel startLabel, meta)
                 , (IRJumpIfZero condExpr endLabel, meta)
                 ] ++ cleanBody ++
                 -- Only add back-edge if we don't break
                 (if not endsWithBreak
                   then [(IRGoto startLabel, meta)]
                   else []) ++
                 [ (IRLabel endLabel, meta)
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
            (breakStmt@(IRGoto label, _):rest) | isEndLabel label ->
                beforeBreak ++ [breakStmt]  -- Drop everything after break
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
                Nothing    -> IRTypeInt32
            Lit (FloatLit _ mtype) ->
              case mtype of
                Just Float32 -> IRTypeFloat32
                Just Float64 -> IRTypeFloat64
                Nothing      -> IRTypeFloat32
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
      _      -> (:[]) <$> convertExpr symTable e

--------------------------------------------------------------------------------
--            convertBlock (unchanged except minor details)
--------------------------------------------------------------------------------

convertBlock
  :: SymbolTable
  -> Expr
  -> Maybe LoopContext
  -> Either IRConversionError [(IRStmt, IRMetadata)]
convertBlock symTable (Block _ blockExprs _) ctx = do
    stmtsLists <- mapM (\e -> convertTop symTable (TLExpr e) ctx) blockExprs
    pure $ concat stmtsLists

convertBlock symTable expr _ = do
    single <- convertExpr symTable expr
    pure [single]

--------------------------------------------------------------------------------
--             convertExpr logic (unchanged except for If case)
--------------------------------------------------------------------------------

convertExpr
  :: SymbolTable
  -> Expr
  -> Either IRConversionError (IRStmt, IRMetadata)
convertExpr symTable (Call "print" [arg]) = do
    -- Unchanged printing logic
    traceM $ "Converting print expression with arg: " ++ show arg
    case arg of
      BinOp op e1 e2 | op `elem` [Add,Sub,Mul,Div] -> do
        left  <- convertToLiteral e1
        right <- convertToLiteral e2
        result <- evalBinOp op left right
        let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
        pure (IRProcCall "print" [IRLit result], meta)
      Call fname args -> do
        convArgs <- mapM (convertToIRExprWithSymbols symTable) args
        let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
        pure (IRProcCall "print" [IRCall fname convArgs], meta)
      FieldAccess b f -> do
        conv    <- convertToIRExprWithSymbols symTable (FieldAccess b f)
        let meta = mkMetadata IRTypeFloat32 (S.singleton IOEffect)
        pure (IRProcCall "print" [conv], meta)
      Var n -> do
        let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
        pure (IRProcCall "print" [IRVar n], meta)
      _ -> do
        conv    <- convertToIRExprWithSymbols symTable arg
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
evalBinOp Add (IRInt32Lit x) (IRInt32Lit y)   = Right $ IRInt32Lit (x + y)
evalBinOp Add (IRInt64Lit x) (IRInt64Lit y)   = Right $ IRInt64Lit (x + y)
evalBinOp Add (IRFloat32Lit x) (IRFloat32Lit y) = Right $ IRFloat32Lit (x + y)
evalBinOp Add (IRFloat64Lit x) (IRFloat64Lit y) = Right $ IRFloat64Lit (x + y)
evalBinOp Sub (IRInt32Lit x) (IRInt32Lit y)   = Right $ IRInt32Lit (x - y)
evalBinOp Sub (IRInt64Lit x) (IRInt64Lit y)   = Right $ IRInt64Lit (x - y)
evalBinOp Sub (IRInt64Lit x) (IRInt32Lit y)   = Right $ IRInt64Lit (x - fromIntegral y)
evalBinOp Sub (IRInt32Lit x) (IRInt64Lit y)   = Right $ IRInt64Lit (fromIntegral x - y)
evalBinOp Sub (IRFloat32Lit x) (IRFloat32Lit y) = Right $ IRFloat32Lit (x - y)
evalBinOp Sub (IRFloat64Lit x) (IRFloat64Lit y) = Right $ IRFloat64Lit (x - y)
evalBinOp Mul (IRInt32Lit x) (IRInt32Lit y)   = Right $ IRInt32Lit (x * y)
evalBinOp Mul (IRInt64Lit x) (IRInt64Lit y)   = Right $ IRInt64Lit (x * y)
evalBinOp Div (IRInt32Lit x) (IRInt32Lit y)
  | y == 0    = Left $ IRUnsupportedLiteral "Division by zero"
  | otherwise = Right $ IRInt32Lit (x `div` y)
evalBinOp Div (IRInt64Lit x) (IRInt64Lit y)
  | y == 0    = Left $ IRUnsupportedLiteral "Division by zero"
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
              if read val > (2^31 - 1)
                 then Right $ IRInt64Lit (read val)
                 else Right $ IRInt32Lit (read val)
        FloatLit val mtype ->
          case mtype of
            Just Float32 -> Right $ IRFloat32Lit (read val)
            Just Float64 -> Right $ IRFloat64Lit (read val)
            Nothing      -> Right $ IRFloat32Lit (read val)
        StringLit s   -> Right $ IRStringLit s
        BooleanLit _  ->
          Left $ IRUnsupportedLiteral "Boolean literals not yet supported"

    Var name -> Right $ IRVarRef name

    BinOp op e1 e2 -> do
      left  <- convertToLiteral e1
      right <- convertToLiteral e2
      evalBinOp op left right

    _ -> Left $ IRUnsupportedLiteral $
          "Unsupported literal: " ++ show expr


--------------------------------------------------------------------------------
--      Convert Expression to IR (unchanged except for If skip)
--------------------------------------------------------------------------------

convertToIRExpr :: Expr -> Either IRConversionError IRExpr
convertToIRExpr expr = convertToIRExprWithSymbols emptySymbolTable expr

convertToIRExprWithSymbols
  :: SymbolTable
  -> Expr
  -> Either IRConversionError IRExpr
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
          if read val > (2^31 - 1)
             then Right $ IRLit $ IRInt64Lit (read val)
             else Right $ IRLit $ IRInt32Lit (read val)
    FloatLit val mtype ->
      case mtype of
        Just Float32 -> Right $ IRLit $ IRFloat32Lit (read val)
        Just Float64 -> Right $ IRLit $ IRFloat64Lit (read val)
        Nothing      -> Right $ IRLit $ IRFloat32Lit (read val)
    StringLit val -> Right $ IRLit $ IRStringLit val
    BooleanLit val -> Right $ IRLit $ IRBoolLit val

convertToIRExprWithSymbols _ (Var name) =
    Right $ IRVar name

convertToIRExprWithSymbols symTable (Call fname args)
  | isFnameStructConstructor fname = do
      traceM $ "\n=== convertToIRExprWithSymbols: struct constructor ==="
      traceM $ "Constructor name: " ++ fname
      traceM $ "Arguments: " ++ show args
      traceM $ "Symbol table structs: " ++ show (structDefs symTable)

      convertedArgs <- mapM (convertToIRExprWithSymbols symTable) args
      let hasTypeParam = '[' `elem` fname && ']' `elem` fname
      if hasTypeParam
        then do
          let baseName = takeWhile (/= '[') fname
          traceM $ "Base name extracted: " ++ baseName
          let paramType = case dropWhile (/= '[') fname of
                ('[':'i':'3':'2':']':_) -> TypeNum Int32
                ('[':'i':'6':'4':']':_) -> TypeNum Int64
                ('[':'f':'3':'2':']':_) -> TypeNum Float32
                ('[':'f':'6':'4':']':_) -> TypeNum Float64
                _ -> error $ "Invalid type parameter in: " ++ fname
          case args of
            [Lit lit] -> do
              let litType = case lit of
                    IntLit _ (Just t)   -> TypeNum t
                    FloatLit _ (Just t) -> TypeNum t
                    _ -> TypeNum Int32
              traceM $ "Validating struct field type: expected="
                       ++ show paramType ++ ", got=" ++ show litType
              when (litType /= paramType) $
                Left $ IRTypeError (convertType litType)
                                   (convertType paramType)
            _ -> Left $ IRError "Invalid struct initialization"

          let concreteTypeName = getSpecializedName baseName paramType
          traceM $ "Generated concrete type name: " ++ concreteTypeName

          case M.lookup baseName (structNames symTable) of
            Just baseId ->
              case lookupStruct baseId symTable of
                Just baseDef -> do
                  let (_, newSymTable) =
                        registerSpecializedStruct concreteTypeName
                            baseDef [paramType] symTable
                  Right $ IRCall "struct_lit"
                     (IRLit (IRStringLit concreteTypeName) : convertedArgs)
                Nothing ->
                  Left $ IRError $ "Invalid struct type: " ++ baseName
            Nothing ->
              Left $ IRError $ "Undefined struct: " ++ baseName
        else
          case args of
            [arg] -> do
              traceM $ "\n=== Checking specialized struct constructor ==="
              traceM $ "Looking up name: " ++ fname
              traceM $ "Current struct names: "
                       ++ show (M.toList $ structNames symTable)
              case M.lookup fname (structNames symTable) of
                Just sid ->
                  case lookupStruct sid symTable of
                    Just def ->
                      if null (structParams def)
                        then do
                          traceM $ "Found existing specialized type: " ++ fname
                          Right $ IRCall "struct_lit"
                            (IRLit (IRStringLit fname) : convertedArgs)
                        else do
                          let paramType = case arg of
                                Lit (IntLit _ (Just Int32))   -> TypeNum Int32
                                Lit (IntLit _ (Just Int64))   -> TypeNum Int64
                                Lit (FloatLit _ (Just Float32))->TypeNum Float32
                                Lit (FloatLit _ (Just Float64))->TypeNum Float64
                                _ -> TypeNum Int32
                          let specializedName = getSpecializedName fname paramType
                          traceM $ "Specializing generic type: "
                                ++ fname ++ " -> " ++ specializedName
                          case M.lookup specializedName (structNames symTable) of
                            Just specSid ->
                              Right $ IRCall "struct_lit"
                                (IRLit (IRStringLit specializedName)
                                  : convertedArgs)
                            Nothing -> do
                              let (_, newSymTable) =
                                   registerSpecializedStruct specializedName
                                     def [paramType] symTable
                              traceM $ "Created specialized struct: "
                                       ++ specializedName
                              Right $ IRCall "struct_lit"
                                (IRLit (IRStringLit specializedName)
                                  : convertedArgs)
                    Nothing ->
                      Left $ IRError $ "Invalid struct type: " ++ fname
                Nothing ->
                  Left $ IRError $ "Undefined struct: " ++ fname
            _ ->
              Right $ IRCall "struct_lit"
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
    left  <- convertToIRExprWithSymbols symTable e1
    right <- convertToIRExprWithSymbols symTable e2
    let result = case op of
          Lt -> IRCall "Lt" [left, right]
          Gt -> IRCall "Gt" [left, right]
          Eq -> IRCall "Eq" [left, right]
          _  -> IRCall (opToString op) [left, right]
    Right result

convertToIRExprWithSymbols symTable (FieldAccess expr field) = do
    traceM $ "\n=== convertToIRExprWithSymbols: FieldAccess ==="
    convBase <- convertToIRExprWithSymbols symTable expr
    let result = IRCall "field_access" [convBase, IRLit (IRStringLit field)]
    Right result

convertToIRExprWithSymbols symTable (StructLit name fields) = do
    traceM $ "\n=== convertToIRExprWithSymbols: StructLit ==="
    let dummyId = StructId 0
    convFields <- mapM (\(f,e) -> do
                       i <- convertToIRExprWithSymbols symTable e
                       pure (f,i)) fields
    Right $ IRCall "struct_lit"
      (IRLit (IRStringLit name) :
       concatMap (\(f,i) -> [IRLit (IRStringLit f), i]) convFields)

convertToIRExprWithSymbols _ (If _ _ _) = do
    traceM "Skipping If expression in convertToIRExprWithSymbols - already handled"
    Right $ IRLit (IRInt32Lit 0)

convertToIRExprWithSymbols _ e = do
    traceM $ "Encoutnered unsupported expression inside convertToIRExprWithSymbols: "
             ++ show e
    Left $ IRUnsupportedExpr $ "Unsupported expression: " ++ show e

--------------------------------------------------------------------------------
-- Helpers for struct detection, operator conversion, etc.
--------------------------------------------------------------------------------

isFnameStructConstructor :: String -> Bool
isFnameStructConstructor nm =
    not (null nm) && C.isUpper (head nm)

opToString :: Op -> String
opToString Add = "Add"
opToString Sub = "Sub"
opToString Mul = "Mul"
opToString Div = "Div"
opToString Lt  = "Lt"
opToString Gt  = "Gt"
opToString Eq  = "Eq"
opToString NotEq = "NotEq"
opToString op  = error $ "Unsupported operator: " ++ show op

--------------------------------------------------------------------------------
--            Type Constraints & Unification (unchanged)
--------------------------------------------------------------------------------

-- | Generate constraints from IR program
generateConstraints :: IRProgram -> Either TypeError [TypeConstraint]
generateConstraints (IRProgram funcs) = do
  concat <$> mapM genFuncConstraints funcs
  where
    genFuncConstraints :: (IRFuncDecl, IRMetadata) -> Either TypeError [TypeConstraint]
    genFuncConstraints (func, _) = do
      bodyConstraints <- genBlockConstraints (fnBody func)
      -- Only add parameter constraints, the body will constrain the return type
      let paramConstraints = map (\(_, t) -> TEq t IRTypeInt32) (fnParams func)
      return $ paramConstraints ++ bodyConstraints

genBlockConstraints :: IRBlock -> Either TypeError [TypeConstraint]
genBlockConstraints (IRBlock _ stmts) =
  concat <$> mapM genStmtConstraints stmts

genStmtConstraints :: (IRStmt, IRMetadata) -> Either TypeError [TypeConstraint]
genStmtConstraints (stmt, _) = case stmt of
  IRStmtExpr expr -> genExprConstraints expr
  IRReturn mexpr -> case mexpr of
    Just expr -> do
      -- Get parent function's return type from context
      exprConstraints <- genExprConstraints expr
      -- Add constraint that return value matches function return type
      return $ TEq IRTypeInt32 IRTypeInt32 : exprConstraints  -- Add return type constraint here
    Nothing -> return []

genExprConstraints :: IRExpr -> Either TypeError [TypeConstraint]
genExprConstraints (IRCall name args) = do
    case (name, args) of
        ("field_access", [expr, IRLit (IRStringLit field)]) -> do
            exprType <- exprType expr
            case exprType of
                IRTypeStruct sname sid ->
                  return [TEq exprType (IRTypeStruct sname sid)]
                _ -> return []  -- We'll enhance type checking for fields later

        ("struct_lit", IRLit (IRStringLit name):fields) -> do
            -- Generate constraints for field types
            let dummyId = StructId 0  -- TODO: Get real ID from symbol table
            fieldConstraints <- concat <$> mapM genExprConstraints fields
            return $ TEq (IRTypeStruct name dummyId) (IRTypeStruct name dummyId) : fieldConstraints
        ("Add", [left, right]) -> do
            traceM "Processing Add operation"
            leftType <- exprType left
            rightType <- exprType right
            traceM $ "Left type: " ++ show leftType
            traceM $ "Right type: " ++ show rightType
            -- Generate constraints without validation
            return [ TEq leftType rightType
                  , TEq (IRTypeVar (TypeVar 0)) leftType ]
        ("Mul", [left, right]) -> do
            traceM "Processing Mul operation"
            leftType <- exprType left
            rightType <- exprType right
            traceM $ "Left type: " ++ show leftType
            traceM $ "Right type: " ++ show rightType
            -- Generate constraints without validation
            return [ TEq leftType rightType
                  , TEq (IRTypeVar (TypeVar 0)) leftType ]
        _ -> do
            -- For other operators, collect type constraints without checking
            argTypes <- mapM exprType args
            return $ map (\t -> TEq t IRTypeInt32) argTypes
  where
    -- Helper to determine numeric type based on literal
    exprNumType :: IRExpr -> IRType
    exprNumType (IRLit (IRFloat32Lit _)) = IRTypeFloat32
    exprNumType (IRLit (IRFloat64Lit _)) = IRTypeFloat64
    exprNumType _ = IRTypeInt32

isNumericType :: IRType -> Bool
isNumericType = \case
    IRTypeInt32 -> True
    IRTypeInt64 -> True
    IRTypeFloat32 -> True
    IRTypeFloat64 -> True
    _ -> False

promoteTypes :: IRType -> IRType -> Either TypeError IRType
promoteTypes IRTypeInt32 IRTypeFloat32 = Right IRTypeFloat32
promoteTypes IRTypeFloat32 IRTypeInt32 = Right IRTypeFloat32
promoteTypes t1 t2 | t1 == t2 = Right t1
promoteTypes t1 t2 = Left $ UnificationError t1 t2

exprType :: IRExpr -> Either TypeError IRType
exprType = \case
    IRLit lit -> do
        traceM $ "Determining literal type: " ++ show lit
        Right $ literalType lit
    IRVar _ -> do
        traceM "Variable reference found"
        Right IRTypeInt32
    IRCall name args -> do
        traceM $ "Processing call: " ++ name ++ " with " ++ show (length args) ++ " args"
        Right IRTypeInt32

literalType :: IRLiteral -> IRType
literalType (IRInt32Lit _) = IRTypeInt32
literalType (IRInt64Lit _) = IRTypeInt64
literalType (IRFloat32Lit _) = IRTypeFloat32
literalType (IRFloat64Lit _) = IRTypeFloat64
literalType (IRStringLit _) = IRTypeString
literalType (IRVarRef _) = IRTypeInt32

-- | Solve type constraints using Robinson's unification algorithm
solveConstraints :: [TypeConstraint] -> Either TypeError TypeSubst
solveConstraints constraints = do
    traceM $ "\n=== Solving Constraints ==="
    traceM $ "Initial constraints: " ++ show constraints
    result <- foldM unifyConstraint M.empty constraints
    traceM $ "Final substitution: " ++ show result
    return result
  where
    unifyConstraint :: TypeSubst -> TypeConstraint -> Either TypeError TypeSubst
    unifyConstraint subst constraint = do
      traceM $ "\nUnifying constraint: " ++ show constraint
      traceM $ "Current substitution: " ++ show subst
      case constraint of
          TEq t1 t2 -> do
              let t1' = applySubst subst t1
              let t2' = applySubst subst t2
              traceM $ "Unified types: " ++ show t1' ++ " = " ++ show t2'
              s <- unify t1' t2'
              traceM $ "New substitution: " ++ show s
              return $ composeSubst s subst
          TVar var t -> do
            let t' = applySubst subst t
            traceM $ "Unified type: " ++ show t'
            s <- unify (IRTypeVar var) t'
            traceM $ "New substitution: " ++ show s
            return $ composeSubst s subst
          TFunc var params ret -> do
            let params' = map (applySubst subst) params
            traceM $ "Params type: " ++ show params'
            let ret' = applySubst subst ret
            traceM $ "Ret type: " ++ show ret'
            s <- unify (IRTypeVar var) (IRTypeFunc params' ret')
            traceM $ "New substitution: " ++ show s
            return $ composeSubst s subst

-- | Core unification algorithm
unify :: IRType -> IRType -> Either TypeError TypeSubst
unify t1 t2 | t1 == t2 = Right M.empty  -- Equal types, no substitution needed
unify (IRTypeVar v) t = bindVar v t
unify t (IRTypeVar v) = bindVar v t
unify (IRTypeFunc params1 ret1) (IRTypeFunc params2 ret2)
  | length params1 == length params2 = do
      -- Unify parameter types
      s1 <- foldM unifyParams M.empty (zip params1 params2)
      -- Unify return type with substitutions applied
      let ret1' = applySubst s1 ret1
      let ret2' = applySubst s1 ret2
      s2 <- unify ret1' ret2'
      return $ composeSubst s2 s1
  where
    unifyParams s (p1, p2) = do
      let p1' = applySubst s p1
      let p2' = applySubst s p2
      s' <- unify p1' p2'
      return $ composeSubst s' s
unify t1 t2 =
    -- First validate numeric types
    if not (isNumericType t1 && isNumericType t2)
    then Left $ UnificationError t1 t2
    -- If valid, attempt coercion
    else coerceTypes t1 t2 >>= \resultType ->
        Right $ M.singleton (TypeVar 0) resultType

coerceTypes :: IRType -> IRType -> Either TypeError IRType
coerceTypes t1 t2 = case (getNumericPrecision t1, getNumericPrecision t2) of
    (Just p1, Just p2) -> Right $ highestPrecisionType t1 t2
    _ -> Left $ UnificationError t1 t2
  where
    highestPrecisionType :: IRType -> IRType -> IRType
    highestPrecisionType a b
        | getNumericPrecision a > getNumericPrecision b = a
        | getNumericPrecision b > getNumericPrecision a = b
        | otherwise = case (a, b) of
            (IRTypeInt32, IRTypeFloat32) -> IRTypeFloat32
            (IRTypeFloat32, IRTypeInt32) -> IRTypeFloat32
            (IRTypeInt64, IRTypeFloat64) -> IRTypeFloat64
            (IRTypeFloat64, IRTypeInt64) -> IRTypeFloat64
            _ -> a -- Same precision, same type

getNumericPrecision :: IRType -> Maybe NumericPrecision
getNumericPrecision = \case
    IRTypeInt32 -> Just P32
    IRTypeFloat32 -> Just P32
    IRTypeInt64 -> Just P64
    IRTypeFloat64 -> Just P64
    _ -> Nothing

-- | Try to bind a type variable to a type, checking for infinite types
bindVar :: TypeVar -> IRType -> Either TypeError TypeSubst
bindVar v t
  | t == IRTypeVar v = Right M.empty  -- v = v
  | occursCheck v t = Left $ InfiniteType v t  -- Infinite type
  | otherwise = Right $ M.singleton v t

-- | Check if type variable appears in type (occurs check)
occursCheck :: TypeVar -> IRType -> Bool
occursCheck v = \case
  IRTypeVar v' -> v == v'
  IRTypeFunc params ret ->
    any (occursCheck v) params || occursCheck v ret
  _ -> False

-- | Apply substitution to a type
applySubst :: TypeSubst -> IRType -> IRType
applySubst s = \case
  t@(IRTypeVar v) -> M.findWithDefault t v s
  IRTypeFunc params ret ->
    IRTypeFunc (map (applySubst s) params) (applySubst s ret)
  t -> t

-- | Compose two substitutions
composeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeSubst s1 s2 = M.union
  (M.map (applySubst s1) s2)
  s1
