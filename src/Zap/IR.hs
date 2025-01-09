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
  , convertToIR'
  , LiteralType(..)
  , TypeError(..)
  , TypeVar(..)
  , TypeConstraint(..)
  , TypeSubst
  , generateConstraints
  , solveConstraints
  , applySubst
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (foldM, unless, when)
import Control.Monad.Except
import Debug.Trace

import Zap.AST as A

-- | Metadata for tracking effects and source info
data LiteralType
  = LitInt NumType
  | LitFloat NumType
  | LitString
  | LitBoolean
  deriving (Show, Eq)

data IRMetadata = IRMetadata
  { metaType :: IRType              -- Type information
  , metaEffects :: S.Set Effect     -- Effect tracking
  , metaSourcePos :: Maybe (Int, Int) -- Source location
  , metaLiteralType :: Maybe LiteralType
  } deriving (Show, Eq)

-- | Effects that nodes can have
data Effect
  = ReadEffect
  | WriteEffect
  | IOEffect
  | PureEffect
  deriving (Show, Eq, Ord)

-- | IR Program with metadata
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

-- Statements with metadata
data IRStmt
  = IRStmtExpr IRExpr     -- Expression statement
  | IRReturn (Maybe IRExpr) -- Return statement
  | IRVarDecl             -- Variable declarations
      { varDeclName :: String
      , varDeclType :: IRType
      , varDeclInit :: IRExpr
      }
  | IRAssign String IRExpr  -- Assignment
  | IRAssignOp String Op IRExpr  -- Compound assignment
  | IRLabel String          -- Label definition
  | IRGoto String          -- Unconditional jump
  | IRJumpIfTrue IRExpr String
  | IRJumpIfZero IRExpr String  -- Conditional jump if expr is zero
  | IRProcCall String [IRExpr]
  deriving (Show, Eq)

-- Expressions with metadata attached
data IRExpr
  = IRCall String [IRExpr]  -- Function call or operator
  | IRVar String           -- Variable reference
  | IRLit IRLiteral        -- Literal value
  deriving (Show, Eq)

data IRLiteral
  = IRStringLit String
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
  | IRTypeStruct String [(String, IRType)]
  deriving (Show, Eq)

data NumericPrecision = P32 | P64 deriving (Eq, Ord)

data IRConversionError
  = IRError String  -- Basic conversion errors
  | IRTypeError IRType IRType -- Type mismatches
  | IRUnsupportedExpr String  -- Unsupported expressions
  | IRUnsupportedLiteral String -- Unsupported literals
  | IRInvalidFunction String  -- Invalid function name/call
  | IRMissingMain            -- No main function found
  deriving (Show, Eq)

-- | Type variables for unification
newtype TypeVar = TypeVar Int
  deriving (Eq, Ord, Show)

-- | Type constraints from inference
data TypeConstraint
  = TEq IRType IRType        -- t1 = t2
  | TVar TypeVar IRType      -- α = t
  | TFunc TypeVar [IRType] IRType  -- α = (t1,...,tn) -> t
  deriving (Eq, Show)

-- | Type substitution mapping
type TypeSubst = M.Map TypeVar IRType

-- | Type inference errors
data TypeError
  = UnificationError IRType IRType
  | InfiniteType TypeVar IRType
  | UnboundVariable T.Text
  deriving (Show, Eq)

-- Helper for creating metadata
mkMetadata :: IRType -> S.Set Effect -> IRMetadata
mkMetadata typ effs = IRMetadata
  { metaType = typ
  , metaEffects = effs
  , metaSourcePos = Nothing
  , metaLiteralType = Nothing
  }

mkLiteralMetadata :: IRType -> S.Set Effect -> LiteralType -> IRMetadata
mkLiteralMetadata typ effs litType = IRMetadata
  { metaType = typ
  , metaEffects = effs
  , metaSourcePos = Nothing
  , metaLiteralType = Just litType
  }

-- Loop context to track break targets
data LoopContext = LoopContext
  { loopNumber :: Int
  , loopEndLabel :: String
  } deriving (Show, Eq)

convertToIR' :: Program -> Either IRConversionError IRProgram
convertToIR' (Program tops) = do
    traceM "\n=== Converting Program to IR ==="

    -- First collect all declarations, including structs
    let (structDefs, restTops) = partitionStructs tops
    traceM $ "Found struct definitions: " ++ show structDefs

    -- Then collect function declarations
    let (funcs, exprs) = partitionFuncs restTops
    traceM $ "Found functions: " ++ show funcs
    traceM $ "Found expressions: " ++ show exprs

    convertedFuncs <- mapM convertFuncDecl funcs
    (mainBlock, mainMeta) <- convertTops exprs Nothing
    let mainFunc = (IRFuncDecl
          { fnName = "main"
          , fnParams = []
          , fnRetType = IRTypeVoid
          , fnBody = mainBlock
          }, mainMeta)

    return $ IRProgram (convertedFuncs ++ [mainFunc])
  where
    -- Split out struct definitions
    partitionStructs :: [TopLevel] -> ([(String, Type)], [TopLevel])
    partitionStructs = foldr splitStruct ([], [])
      where
        splitStruct (TLType name t@(TypeStruct _ _)) (ss, ts) = ((name, t):ss, ts)
        splitStruct t (ss, ts) = (ss, t:ts)

    -- Split out function declarations
    partitionFuncs :: [TopLevel] -> ([Decl], [TopLevel])
    partitionFuncs = foldr splitFunc ([], [])
      where
        splitFunc (TLDecl d@(DFunc _ _ _ _)) (fs, ts) = (d:fs, ts)
        splitFunc t (fs, ts) = (fs, t:ts)

convertStructType :: Type -> Either IRConversionError IRType
convertStructType (TypeStruct name fields) =
    Right $ IRTypeStruct name [(fname, convertType ftype) | (fname, ftype) <- fields]
convertStructType t = Right $ convertType t -- Fall back to existing type conversion

-- | Convert function declarations to IR
convertFuncDecl :: Decl -> Either IRConversionError (IRFuncDecl, IRMetadata)
convertFuncDecl (DFunc name params retType body) = do
    traceM $ "\n=== Converting function: " ++ name
    traceM $ "Parameters: " ++ show params
    traceM $ "Return type: " ++ show retType

    -- Convert parameters to IR types
    let irParams = [(pname, convertType ptyp) | Param pname ptyp <- params]

    -- Handle function body
    case body of
        Block scope -> do
            -- Get the expressions from the scope
            let bodyExprs = blockExprs scope

            -- Convert each expression to IR statements
            convertedStmts <- concat <$> mapM convertExprToStmts bodyExprs

            -- Handle the return value
            returnStmt <- case bodyExprs of
                [] -> return [(IRReturn Nothing, mkMetadata (convertType retType) (S.singleton PureEffect))]
                exprs -> do
                    -- Convert last expression for return
                    lastExpr <- convertToIRExpr (last exprs)
                    return [(IRReturn (Just lastExpr), mkMetadata (convertType retType) (S.singleton PureEffect))]

            let bodyBlock = IRBlock "function.entry" (convertedStmts ++ returnStmt)

            return (IRFuncDecl
                { fnName = name
                , fnParams = irParams
                , fnRetType = convertType retType
                , fnBody = bodyBlock
                }, mkMetadata (convertType retType) (S.singleton PureEffect))

        _ -> Left $ IRUnsupportedExpr $ "Function body must be a block: " ++ show body

-- Helper to convert expressions to IR statements
convertExprToStmts :: Expr -> Either IRConversionError [(IRStmt, IRMetadata)]
convertExprToStmts expr = do
    traceM $ "=== Converting expression to statements: " ++ show expr
    case expr of
        Let name val -> do
            traceM $ "Converting Let binding for: " ++ name
            convertedExpr <- convertToIRExpr val
            -- Extract literal type from value if present
            let meta = case val of
                  Lit (IntLit _ mtype) ->
                    let irType = case mtype of
                            Just Int32 -> IRTypeInt32
                            Just Int64 -> IRTypeInt64
                            Nothing -> IRTypeInt32  -- Default
                        litType = case mtype of
                            Just nt -> LitInt nt
                            Nothing -> LitInt Int32
                    in mkLiteralMetadata irType (S.singleton WriteEffect) litType
                  Lit (FloatLit _ mtype) ->
                    let irType = case mtype of
                            Just Float32 -> IRTypeFloat32
                            Just Float64 -> IRTypeFloat64
                            Nothing -> IRTypeFloat32
                        litType = case mtype of
                            Just nt -> LitFloat nt
                            Nothing -> LitFloat Float32
                    in mkLiteralMetadata irType (S.singleton WriteEffect) litType
                  Lit (StringLit _) ->
                    mkLiteralMetadata IRTypeString (S.singleton WriteEffect) LitString
                  _ ->
                    mkMetadata IRTypeVoid (S.singleton WriteEffect)
            traceM $ "Meta type: " ++ show (metaType meta)
            -- For struct literals, extract proper type
            let irType = case val of
                  StructLit typeName fields ->
                    IRTypeStruct typeName [(fname, typeFromExpr fexpr) | (fname, fexpr) <- fields]
                  _ -> IRTypeVoid  -- Default case
            traceM $ "Assigned type: " ++ show irType
            let stmt = IRVarDecl name (metaType meta) convertedExpr
            traceM $ "Created statement: " ++ show stmt
            return [(IRVarDecl name irType convertedExpr, meta)]
        BinOp op e1 e2 -> do
            traceM $ "Converting binary operation: " ++ show op
            left <- convertToIRExpr e1
            right <- convertToIRExpr e2
            -- Create IR binary operation call
            let opStr = case op of
                    Add -> "Add"
                    Sub -> "Sub"
                    Mul -> "Mul"
                    Div -> "Div"
                    _ -> error $ "Unsupported operator: " ++ show op
            return [(IRStmtExpr (IRCall opStr [left, right]),
                    mkMetadata IRTypeInt32 (S.singleton PureEffect))]
        Call "print" [arg] -> do
            traceM $ "=== Converting print statement ==="
            traceM $ "Input arg: " ++ show arg
            converted <- convertToIRExpr arg
            traceM $ "Converted arg: " ++ show converted
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            let stmt = (IRProcCall "print" [converted], meta)
            traceM $ "Generated statement: " ++ show stmt
            return [stmt]
        _ -> do
            converted <- convertToIRExpr expr
            return [(IRStmtExpr converted, mkMetadata IRTypeVoid (S.singleton PureEffect))]

-- Helper to determine IR type from expression
typeFromExpr :: Expr -> IRType
typeFromExpr (Lit (FloatLit _ (Just Float32))) = IRTypeFloat32
typeFromExpr (Lit (FloatLit _ (Just Float64))) = IRTypeFloat64
typeFromExpr (Lit (IntLit _ (Just Int32))) = IRTypeInt32
typeFromExpr (Lit (IntLit _ (Just Int64))) = IRTypeInt64
typeFromExpr (StructLit name fields) =
    IRTypeStruct name [(fname, typeFromExpr fexpr) | (fname, fexpr) <- fields]
typeFromExpr _ = IRTypeVoid  -- Default case

-- Helper to convert AST types to IR types
convertType :: Type -> IRType
convertType (TypeNum Int32) = IRTypeInt32
convertType (TypeNum Int64) = IRTypeInt64
convertType TypeVoid = IRTypeVoid
convertType (TypeStruct name fields) =
  IRTypeStruct name [(fname, convertType ftype) | (fname, ftype) <- fields]

convertTops :: [TopLevel] -> Maybe LoopContext -> Either IRConversionError (IRBlock, IRMetadata)
convertTops tops ctx = do
    stmts <- concat <$> mapM (\t -> convertTop t ctx) tops
    -- Get last metadata to preserve literal type info
    let lastMeta = if null stmts
        then mkMetadata IRTypeVoid (S.singleton PureEffect)
        else snd (last stmts)
    let returnStmt = (IRReturn Nothing, mkMetadata IRTypeVoid (S.singleton PureEffect))
    return (IRBlock "main.entry" (stmts ++ [returnStmt]), lastMeta)

convertTop :: TopLevel -> Maybe LoopContext -> Either IRConversionError [(IRStmt, IRMetadata)]
convertTop (TLExpr (If cond thenExpr (BoolLit False))) ctx = do
    let isBreakPattern = case thenExpr of
          Block scope -> any isBreakStmt (blockExprs scope)
          _ -> False

    let targetLabel = case ctx of
          Just loopCtx -> loopEndLabel loopCtx
          Nothing -> "if_end"

    condExpr <- convertToIRExpr cond
    let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)

    if isBreakPattern
        then do
            -- For if/break pattern, jump to loop end if condition is true
            return [ (IRJumpIfTrue condExpr targetLabel, meta)
                   , (IRGoto $ "while_" ++ show nextLoopNum ++ "_start", meta)  -- And this one
                   ]
        else do
            -- Normal if case
            thenStmts <- convertBlock thenExpr ctx
            return $ [ (IRJumpIfZero condExpr targetLabel, meta) ]
                    ++ thenStmts
                    ++ [ (IRLabel targetLabel, meta) ]

  where
    isBreakStmt (Break _) = True
    isBreakStmt _ = False

    -- Get next loop number from context
    nextLoopNum = case ctx of
        Just loopCtx -> loopNumber loopCtx
        Nothing -> 0

convertTop (TLExpr (While cond body)) prevCtx = do
    -- Generate next loop number based on previous context
    let nextNum = case prevCtx of
          Just ctx -> loopNumber ctx + 1
          Nothing -> 0

    -- Generate unique labels using loop number
    let startLabel = "while_" ++ show nextNum ++ "_start"
    let endLabel = "while_" ++ show nextNum ++ "_end"
    let ctx = LoopContext nextNum endLabel

    -- Convert condition and body with new context
    condExpr <- convertToIRExpr cond
    bodyStmts <- convertBlock body (Just ctx)

    -- Remove any trailing goto statements from bodyStmts that would duplicate the loop goto
    let cleanBodyStmts = removeTrailingGoto startLabel bodyStmts

    let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
    return $
        [ (IRLabel startLabel, meta)
        , (IRJumpIfZero condExpr endLabel, meta)
        ] ++
        cleanBodyStmts ++
        [ (IRGoto startLabel, meta)
        , (IRLabel endLabel, meta)
        ]
  where
    -- Helper to remove trailing goto if it matches our loop's start label
    removeTrailingGoto :: String -> [(IRStmt, IRMetadata)] -> [(IRStmt, IRMetadata)]
    removeTrailingGoto label stmts = case reverse stmts of
        (stmt@(IRGoto l, _):rest) | l == label -> reverse rest
        _ -> stmts

convertTop (TLExpr (VarDecl name value)) ctx = do
    traceM $ "Converting top-level variable declaration: " ++ name
    traceM $ "With value: " ++ show value
    convertedExpr <- convertToIRExpr value

    let irType = case value of
          Lit (IntLit _ mtype) ->
            case mtype of
              Just Int32 -> IRTypeInt32
              Just Int64 -> IRTypeInt64
              Nothing -> IRTypeInt32  -- Default
          Lit (FloatLit _ mtype) ->
            case mtype of
              Just Float32 -> IRTypeFloat32
              Just Float64 -> IRTypeFloat64
              Nothing -> IRTypeFloat32
          Lit (StringLit _) -> IRTypeString
          _ -> IRTypeVoid

    -- Create metadata based on value type
    let meta = case value of
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
          _ ->
            mkMetadata IRTypeVoid (S.singleton WriteEffect)

    traceM $ "Using IR type: " ++ show irType
    return [(IRVarDecl name irType convertedExpr, meta)]

convertTop (TLExpr (Break _)) Nothing =
    Left $ IRError "Break statement outside loop"
convertTop (TLExpr (Break _)) (Just ctx) = do
    let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
    return [(IRGoto (loopEndLabel ctx), meta)]

convertTop (TLExpr (Assign name expr)) _ = do
    convertedExpr <- convertToIRExpr expr
    let meta = mkMetadata IRTypeVoid (S.singleton WriteEffect)
    return [(IRAssign name convertedExpr, meta)]

convertTop (TLExpr (AssignOp name op expr)) _ = do
    convertedExpr <- convertToIRExpr expr
    let meta = mkMetadata IRTypeVoid (S.singleton WriteEffect)
    return [(IRAssignOp name op convertedExpr, meta)]

convertTop (TLExpr e) ctx = case e of
    -- Handle Let expression directly
    Let {} -> convertExprToStmts e
    -- Other top-level expressions continue to use convertExpr
    _ -> (:[]) <$> convertExpr e

convertBlock :: Expr -> Maybe LoopContext -> Either IRConversionError [(IRStmt, IRMetadata)]
convertBlock (Block scope) ctx = do
    -- Convert each expression in block with context
    stmtsLists <- mapM (\e -> convertTop (TLExpr e) ctx) (blockExprs scope)
    return $ concat stmtsLists
convertBlock expr _ = do
    -- Single expression blocks
    converted <- convertExpr expr
    return [converted]

convertExpr :: Expr -> Either IRConversionError (IRStmt, IRMetadata)
convertExpr (Call "print" [arg]) = do
    traceM $ "Converting print expression with arg: " ++ show arg
    case arg of
        BinOp op e1 e2 | op `elem` [Add, Sub, Mul, Div] -> do  -- Add Sub to supported ops
            traceM $ "Converting binary operation: " ++ show op
            left <- convertToLiteral e1
            traceM $ "Left operand converted: " ++ show left
            right <- convertToLiteral e2
            traceM $ "Right operand converted: " ++ show right
            result <- evalBinOp op left right
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            return (IRProcCall "print" [IRLit result], meta)
        Call fname args -> do
            convertedArgs <- mapM convertToIRExpr args
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            return (IRProcCall "print" [IRCall fname convertedArgs], meta)
        FieldAccess expr field -> do
            -- Handle field access expression directly
            convertedExpr <- convertToIRExpr (FieldAccess expr field)
            let meta = mkMetadata IRTypeFloat32 (S.singleton IOEffect)
            return (IRProcCall "print" [convertedExpr], meta)
        Var name -> do
            traceM $ "Converting variable reference in print: " ++ name
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            return (IRProcCall "print" [IRVar name], meta)
        _ -> do
            converted <- convertToIRExpr arg
            traceM $ "Converted print arg to: " ++ show converted
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            return (IRProcCall "print" [converted], meta)

convertExpr (Call "print" _) =
    Left $ IRInvalidFunction "print requires exactly one argument"

convertExpr e = Left $ IRUnsupportedExpr $ "Unsupported expression: " ++ show e

-- Helper to evaluate binary operations at compile time
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
evalBinOp Lt (IRInt32Lit x) (IRInt32Lit y) = Right $ IRInt32Lit (if x < y then 1 else 0)
evalBinOp Lt (IRInt64Lit x) (IRInt64Lit y) = Right $ IRInt64Lit (if x < y then 1 else 0)
evalBinOp Gt (IRInt32Lit x) (IRInt32Lit y) = Right $ IRInt32Lit (if x > y then 1 else 0)
evalBinOp Gt (IRInt64Lit x) (IRInt64Lit y) = Right $ IRInt64Lit (if x > y then 1 else 0)
evalBinOp EqEq (IRInt32Lit x) (IRInt32Lit y) = Right $ IRInt32Lit (if x == y then 1 else 0)
evalBinOp EqEq (IRInt64Lit x) (IRInt64Lit y) = Right $ IRInt64Lit (if x == y then 1 else 0)
evalBinOp op _ _ = Left $ IRUnsupportedLiteral $ "Unsupported operator: " ++ show op

-- Helper to convert expressions to literals
convertToLiteral :: Expr -> Either IRConversionError IRLiteral
convertToLiteral expr = case expr of
    Lit lit -> case lit of
        IntLit val mtype ->
            case mtype of
                Just Int32 -> Right $ IRInt32Lit (read val)
                Just Int64 -> Right $ IRInt64Lit (read val)
                Nothing ->
                    -- Fall back to size-based inference if no explicit type
                    if read val > (2^31 - 1)
                    then Right $ IRInt64Lit (read val)
                    else Right $ IRInt32Lit (read val)

        FloatLit val mtype ->
            case mtype of
                Just Float32 -> Right $ IRFloat32Lit (read val)
                Just Float64 -> Right $ IRFloat64Lit (read val)
                Nothing -> Right $ IRFloat32Lit (read val)
        StringLit s -> Right $ IRStringLit s
        BooleanLit _ -> Left $ IRUnsupportedLiteral "Boolean literals not yet supported"
    Var name -> Right $ IRVarRef name
    BinOp op e1 e2 -> do
        left <- convertToLiteral e1
        right <- convertToLiteral e2
        evalBinOp op left right
    e -> Left $ IRUnsupportedLiteral $ "Unsupported literal: " ++ show e

    where
      maxInt32 :: Integer
      maxInt32 = 2^31 - 1

-- Helper to convert expressions to IR
convertToIRExpr :: Expr -> Either IRConversionError IRExpr
convertToIRExpr (VarDecl name value) = do
    traceM $ "Converting variable declaration in expression: " ++ name
    convertedExpr <- convertToIRExpr value
    Right $ IRCall "var_decl" [IRLit (IRStringLit name), convertedExpr]
convertToIRExpr (Lit lit) = case lit of
  IntLit val mtype -> case mtype of
    Just Int32 -> Right $ IRLit $ IRInt32Lit (read val)
    Just Int64 -> Right $ IRLit $ IRInt64Lit (read val)
    Nothing -> -- Use size-based inference for integers
      if read val > (2^31 - 1)
      then Right $ IRLit $ IRInt64Lit (read val)
      else Right $ IRLit $ IRInt32Lit (read val)

  FloatLit val mtype ->
    -- For now, default to Float32 for backwards compatibility
    -- We could later switch to Float64 default or size-based inference
    case mtype of
      Just Float32 -> Right $ IRLit $ IRFloat32Lit (read val)
      Just Float64 -> Right $ IRLit $ IRFloat64Lit (read val)
      Nothing -> Right $ IRLit $ IRFloat32Lit (read val)  -- Default choice
  StringLit val -> Right $ IRLit $ IRStringLit val
  -- BooleanLit val -> Right $ IRLit $ IRBoolLit val
convertToIRExpr (Var name) = Right $ IRVar name
convertToIRExpr (Call fname args) = do
    -- Convert each argument to IR
    convertedArgs <- mapM convertToIRExpr args
    Right $ IRCall fname convertedArgs
convertToIRExpr (Block scope) = do
    -- Handle last expression in block as result
    case blockExprs scope of
        [] -> Left $ IRError "Empty block"
        exprs -> convertToIRExpr (last exprs)
convertToIRExpr (BinOp op e1 e2) = do
    traceM $ "Converting binary operation: " ++ show op
    left <- convertToIRExpr e1
    right <- convertToIRExpr e2
    case op of
      Lt -> Right $ IRCall "Lt" [left, right]
      Gt -> Right $ IRCall "Gt" [left, right]
      EqEq -> Right $ IRCall "EqEq" [left, right]
      _ -> Right $ IRCall (opToString op) [left, right]
convertToIRExpr (FieldAccess expr field) = do
    convertedExpr <- convertToIRExpr expr
    Right $ IRCall "field_access" [convertedExpr, IRLit (IRStringLit field)]
convertToIRExpr (StructLit name fields) = do
    -- Convert each field expression
    convertedFields <- mapM (\(fname, expr) -> do
        irExpr <- convertToIRExpr expr
        return (fname, irExpr)) fields
    -- Create struct literal call with field name/value pairs
    Right $ IRCall "struct_lit" (IRLit (IRStringLit name) :
        concatMap (\(fname, expr) ->
            [IRLit (IRStringLit fname), expr]) convertedFields)

convertToIRExpr (FieldAccess expr field) = do
    convertedExpr <- convertToIRExpr expr
    Right $ IRCall "field_access" [convertedExpr, IRLit (IRStringLit field)]
convertToIRExpr e = Left $ IRUnsupportedExpr $ "Unsupported expression: " ++ show e
convertToIRExpr e = Left $ IRUnsupportedExpr $ "Unsupported expression: " ++ show e

-- Helper for operator conversion
opToString :: Op -> String
opToString Add = "Add"
opToString Sub = "Sub"
opToString Mul = "Mul"
opToString Div = "Div"
opToString Lt = "Lt"
opToString Gt = "Gt"
opToString EqEq = "EqEq"
opToString op = error $ "Unsupported operator: " ++ show op

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
                IRTypeStruct sname fields -> case lookup field fields of
                    Just fieldType -> return [TEq exprType (IRTypeStruct sname fields)]
                    Nothing -> Left $ UnboundVariable $ T.pack $ "Field not found: " ++ field
                _ -> return []  -- We'll enhance type checking for fields later

        ("struct_lit", IRLit (IRStringLit name):fields) -> do
            -- Generate constraints for field types
            fieldConstraints <- concat <$> mapM genExprConstraints fields
            return $ TEq (IRTypeStruct name []) (IRTypeStruct name []) : fieldConstraints
        ("Add", [left, right]) -> do
            traceM "Processing Add operation"
            leftType <- exprType left
            rightType <- exprType right
            traceM $ "Left type: " ++ show leftType
            traceM $ "Right type: " ++ show rightType
            -- Generate constraints without validation
            return [ TEq leftType rightType
                  , TEq (IRTypeVar (TypeVar 0)) leftType ]
            -- -- Validate numeric types first
            -- unless (isNumericType leftType && isNumericType rightType) $ do
            --     traceM $ "Type error: non-numeric types in Add operation"
            --     Left $ UnificationError leftType rightType
            -- let resultVar = TypeVar 0
            -- -- First establish operands must match
            -- let baseConstraints = [TEq leftType rightType]
            -- -- Then determine result type based on operand types
            -- resultType <- case (leftType, rightType) of
            --     (t1, t2) | t1 == t2 -> Right t1  -- Same types preserve precision
            --     _ -> coerceTypes leftType rightType  -- Different types promote
            -- return $ baseConstraints ++
            --         [TEq (IRTypeVar resultVar) resultType]
        ("Mul", [left, right]) -> do
            traceM "Processing Mul operation"
            leftType <- exprType left
            rightType <- exprType right
            traceM $ "Left type: " ++ show leftType
            traceM $ "Right type: " ++ show rightType
            -- Generate constraints without validation
            return [ TEq leftType rightType
                  , TEq (IRTypeVar (TypeVar 0)) leftType ]
            -- -- Validate numeric types first
            -- unless (isNumericType leftType && isNumericType rightType) $ do
            --     traceM $ "Type error: non-numeric types in Add operation"
            --     Left $ UnificationError leftType rightType
            -- let resultVar = TypeVar 1
            -- let baseConstraints = [TEq leftType rightType]
            -- resultType <- case (leftType, rightType) of
            --     (t1, t2) | t1 == t2 -> Right t1
            --     _ -> coerceTypes leftType rightType
            -- return $ baseConstraints ++
            --         [TEq (IRTypeVar resultVar) resultType]
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
