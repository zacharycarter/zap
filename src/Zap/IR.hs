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
  , TypeVar(..)
  , TypeConstraint(..)
  , TypeSubst
  , generateConstraints
  , solveConstraints
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (when)
import Control.Monad.Except
import Debug.Trace

import Zap.AST as A

-- | Metadata for tracking effects and source info
data IRMetadata = IRMetadata
  { metaType :: IRType              -- Type information
  , metaEffects :: S.Set Effect     -- Effect tracking
  , metaSourcePos :: Maybe (Int, Int) -- Source location
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
  | IRIntLit Int
  | IRVarRef String
  deriving (Show, Eq)

data IRType
  = IRTypeVoid
  | IRTypeInt
  | IRTypeInt32
  | IRTypeString
  deriving (Show, Eq)

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
  }

-- Loop context to track break targets
data LoopContext = LoopContext
  { loopNumber :: Int
  , loopEndLabel :: String
  } deriving (Show, Eq)

convertToIR' :: Program -> Either IRConversionError IRProgram
convertToIR' (Program tops) = do
    traceM "\n=== Converting Program to IR ==="
    traceM $ "Top level expressions: " ++ show tops

    -- First collect all function declarations
    let (funcs, exprs) = partitionTops tops
    traceM $ "Found functions: " ++ show funcs
    traceM $ "Found expressions: " ++ show exprs

    -- Convert functions first
    convertedFuncs <- mapM convertFuncDecl funcs

    -- Then convert main block
    mainBlock <- convertTops exprs Nothing
    let mainFunc = (IRFuncDecl
          { fnName = "main"
          , fnParams = []
          , fnRetType = IRTypeVoid
          , fnBody = mainBlock
          }, mkMetadata IRTypeVoid (S.singleton PureEffect))

    return $ IRProgram (convertedFuncs ++ [mainFunc])
  where
    partitionTops :: [TopLevel] -> ([Decl], [TopLevel])
    partitionTops = foldr splitTop ([], [])
      where
        splitTop (TLDecl d@(DFunc _ _ _ _)) (fs, es) = (d:fs, es)
        splitTop e (fs, es) = (fs, e:es)

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
convertExprToStmts expr = case expr of
    BinOp op e1 e2 -> do
        -- For binary operations that will be returned, don't generate a separate statement
        case op of
            Add -> return []  -- We'll handle this in the return statement
            Sub -> return []
            Mul -> return []
            Div -> return []
            _ -> do  -- For other ops like comparisons, keep generating statements
                left <- convertToIRExpr e1
                right <- convertToIRExpr e2
                let stmt = IRStmtExpr $ IRCall (opToString op) [left, right]
                return [(stmt, mkMetadata IRTypeVoid (S.singleton PureEffect))]
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

-- Helper to convert AST types to IR types
convertType :: Type -> IRType
convertType (TypeNum Int32) = IRTypeInt32
convertType TypeVoid = IRTypeVoid
convertType t = IRTypeInt  -- Default for now

convertTops :: [TopLevel] -> Maybe LoopContext -> Either IRConversionError IRBlock
convertTops tops ctx = do
    stmts <- concat <$> mapM (\t -> convertTop t ctx) tops
    let returnStmt = (IRReturn Nothing, mkMetadata IRTypeVoid (S.singleton PureEffect))
    return $ IRBlock "main.entry" (stmts ++ [returnStmt])

convertTop :: TopLevel -> Maybe LoopContext -> Either IRConversionError [(IRStmt, IRMetadata)]
convertTop (TLExpr (If cond thenExpr (BoolLit False))) ctx = do
    -- Generate labels for if block
    let endLabel = "if_end"

    -- Convert condition
    condExpr <- convertToIRExpr cond

    -- We need to jump if false (negate condition)
    let negatedCond = case condExpr of
          IRCall "EqEq" [a, b] ->
              -- x == y becomes !(x == y)
              IRCall "Eq" [a, b]  -- Eq is our not-equal operator
          _ -> IRCall "Eq" [condExpr, IRLit (IRIntLit 1)]  -- Other conditions: jump if == 0

    -- Convert then block
    thenStmts <- convertBlock thenExpr ctx

    let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)

    -- Build if block:
    -- if (!cond) goto end
    -- <then statements>
    -- end:
    return $
        [ (IRJumpIfZero negatedCond endLabel, meta)
        ] ++
        thenStmts ++
        [ (IRLabel endLabel, meta)
        ]
convertTop (TLExpr (VarDecl name (NumLit _ val))) _ = do
    let irType = IRTypeInt32
    let initExpr = IRLit (IRIntLit (read val))
    let meta = mkMetadata irType (S.singleton WriteEffect)
    return [(IRVarDecl name irType initExpr, meta)]

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

    let meta = mkMetadata IRTypeVoid (S.singleton PureEffect)
    return $
        [ (IRLabel startLabel, meta)
        , (IRJumpIfZero condExpr endLabel, meta)
        ] ++
        bodyStmts ++
        [ (IRGoto startLabel, meta)
        , (IRLabel endLabel, meta)
        ]

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

convertTop (TLExpr e) ctx = (:[]) <$> convertExpr e

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
        BinOp op e1 e2 | op `elem` [Add, Mul] -> do
            left <- convertToLiteral e1
            right <- convertToLiteral e2
            result <- evalBinOp op left right
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            return (IRProcCall "print" [IRLit result], meta)  -- Changed to IRProcCall
        Call fname args -> do
            convertedArgs <- mapM convertToIRExpr args
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            return (IRProcCall "print" [IRCall fname convertedArgs], meta)  -- Changed to IRProcCall
        _ -> do
            -- Original string literal case
            lit <- convertToLiteral arg
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            return (IRProcCall "print" [IRLit lit], meta)  -- Changed to IRProcCall

convertExpr (Call "print" _) =
    Left $ IRInvalidFunction "print requires exactly one argument"

convertExpr e = Left $ IRUnsupportedExpr $ "Unsupported expression: " ++ show e

-- Helper to evaluate binary operations at compile time
evalBinOp :: Op -> IRLiteral -> IRLiteral -> Either IRConversionError IRLiteral
evalBinOp Add (IRIntLit x) (IRIntLit y) = Right $ IRIntLit (x + y)
evalBinOp Sub (IRIntLit x) (IRIntLit y) = Right $ IRIntLit (x - y)
evalBinOp Mul (IRIntLit x) (IRIntLit y) = Right $ IRIntLit (x * y)
evalBinOp Div (IRIntLit x) (IRIntLit y)
  | y == 0 = Left $ IRUnsupportedLiteral "Division by zero"
  | otherwise = Right $ IRIntLit (x `div` y)
evalBinOp Lt (IRIntLit x) (IRIntLit y) = Right $ IRIntLit (if x < y then 1 else 0)
evalBinOp Gt (IRIntLit x) (IRIntLit y) = Right $ IRIntLit (if x > y then 1 else 0)
evalBinOp EqEq (IRIntLit x) (IRIntLit y) = Right $ IRIntLit (if x == y then 1 else 0)
evalBinOp op _ _ = Left $ IRUnsupportedLiteral $ "Unsupported operator: " ++ show op

-- Helper to convert expressions to literals
convertToLiteral :: Expr -> Either IRConversionError IRLiteral
convertToLiteral (StrLit s) = Right $ IRStringLit s
convertToLiteral (NumLit _ n) = Right $ IRIntLit (read n)
convertToLiteral (BinOp op e1 e2) = do
    left <- convertToLiteral e1
    right <- convertToLiteral e2
    evalBinOp op left right
convertToLiteral (Var name) = Right $ IRVarRef name
convertToLiteral e = Left $ IRUnsupportedLiteral $ "Unsupported literal: " ++ show e

-- Helper to convert expressions to IR
convertToIRExpr :: Expr -> Either IRConversionError IRExpr
convertToIRExpr (NumLit _ val) = Right $ IRLit (IRIntLit (read val))
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
    left <- convertToIRExpr e1
    right <- convertToIRExpr e2
    Right $ IRCall (opToString op) [left, right]
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
      -- Generate constraints from function body
      bodyConstraints <- genBlockConstraints (fnBody func)

      -- Add constraint for return type
      let retConstraint = TEq (fnRetType func) (IRTypeInt) -- For now, assuming int return

      -- Add constraints for parameters
      let paramConstraints = map (\(_, t) -> TEq t IRTypeInt) (fnParams func)

      return $ retConstraint : paramConstraints ++ bodyConstraints

genBlockConstraints :: IRBlock -> Either TypeError [TypeConstraint]
genBlockConstraints (IRBlock _ stmts) =
  concat <$> mapM genStmtConstraints stmts

genStmtConstraints :: (IRStmt, IRMetadata) -> Either TypeError [TypeConstraint]
genStmtConstraints (stmt, _) = case stmt of
  IRStmtExpr expr -> genExprConstraints expr
  IRReturn mexpr -> case mexpr of
    Just expr -> genExprConstraints expr
    Nothing -> return []

genExprConstraints :: IRExpr -> Either TypeError [TypeConstraint]
genExprConstraints (IRCall name args) = do
    let argConstraints = map exprType args
    return $ map (\t -> TEq t IRTypeInt) argConstraints
  where
    exprType :: IRExpr -> IRType
    exprType (IRLit lit) = literalType lit
    exprType (IRVar _) = IRTypeInt
    exprType (IRCall _ _) = IRTypeInt

literalType :: IRLiteral -> IRType
literalType (IRIntLit _) = IRTypeInt
literalType (IRStringLit _) = IRTypeString

-- | Solve type constraints
solveConstraints :: [TypeConstraint] -> Either TypeError TypeSubst
solveConstraints = undefined -- TODO: Implement Robinson's algorithm
