{-# LANGUAGE OverloadedStrings #-}

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
  ) where

import qualified Data.Text as T
import qualified Data.Set as S
import Control.Monad (when)
import Control.Monad.Except
import Debug.Trace

import Zap.AST

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
  { blockLabel :: String
  , blockStmts :: [(IRStmt, IRMetadata)]
  } deriving (Show, Eq)

-- Statements with metadata
data IRStmt
  = IRStmtExpr IRExpr
  | IRReturn (Maybe IRExpr)
  deriving (Show, Eq)

-- Expressions with metadata attached
data IRExpr
  = IRCall String [IRLiteral]
  deriving (Show, Eq)

data IRLiteral
  = IRStringLit String
  | IRIntLit Int
  deriving (Show, Eq)

data IRType
  = IRTypeVoid
  | IRTypeInt
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

-- Helper for creating metadata
mkMetadata :: IRType -> S.Set Effect -> IRMetadata
mkMetadata typ effs = IRMetadata
  { metaType = typ
  , metaEffects = effs
  , metaSourcePos = Nothing -- We can add source positions later
  }

convertToIR' :: Program -> Either IRConversionError IRProgram
convertToIR' (Program tops) = do
    mainBlock <- convertTops tops
    let mainFunc = (IRFuncDecl
          { fnName = "main"
          , fnParams = []
          , fnRetType = IRTypeVoid
          , fnBody = mainBlock
          }, mkMetadata IRTypeVoid (S.singleton PureEffect))
    return $ IRProgram [mainFunc]

convertTops :: [TopLevel] -> Either IRConversionError IRBlock
convertTops tops = do
    stmts <- concat <$> mapM convertTop tops
    let returnStmt = (IRReturn Nothing, mkMetadata IRTypeVoid (S.singleton PureEffect))
    return $ IRBlock "main.entry" (stmts ++ [returnStmt])

convertTop :: TopLevel -> Either IRConversionError [(IRStmt, IRMetadata)]
convertTop (TLExpr e) = (:[]) <$> convertExpr e
convertTop _ = Right []

convertExpr :: Expr -> Either IRConversionError (IRStmt, IRMetadata)       
convertExpr (Call "print" [arg]) = do
    -- For numeric expressions, convert the expression itself
    case arg of
        BinOp op e1 e2 | op `elem` [Add, Mul] -> do
            left <- convertLiteral e1
            right <- convertLiteral e2
            result <- evalBinOp op left right
            let stmt = IRStmtExpr $ IRCall "print" [result]
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            return (stmt, meta)
        _ -> do
            -- Original string literal case
            lit <- convertLiteral arg
            let stmt = IRStmtExpr $ IRCall "print" [lit]
            let meta = mkMetadata IRTypeVoid (S.singleton IOEffect)
            return (stmt, meta)

convertExpr (Call "print" _) =
    Left $ IRInvalidFunction "print requires exactly one argument"

convertExpr e = Left $ IRUnsupportedExpr $ "Unsupported expression: " ++ show e

-- Helper to evaluate binary operations at compile time
evalBinOp :: Op -> IRLiteral -> IRLiteral -> Either IRConversionError IRLiteral
evalBinOp Add (IRIntLit x) (IRIntLit y) = Right $ IRIntLit (x + y)
evalBinOp Mul (IRIntLit x) (IRIntLit y) = Right $ IRIntLit (x * y)
evalBinOp op _ _ = Left $ IRUnsupportedLiteral $ "Unsupported operator: " ++ show op

convertLiteral :: Expr -> Either IRConversionError IRLiteral
convertLiteral (StrLit s) = Right $ IRStringLit s
convertLiteral (NumLit _ n) = Right $ IRIntLit (read n)
convertLiteral (BinOp op e1 e2) = do
    left <- convertLiteral e1
    right <- convertLiteral e2
    evalBinOp op left right
convertLiteral e = Left $ IRUnsupportedLiteral $ "Unsupported literal: " ++ show e

-- {-# LANGUAGE OverloadedStrings #- }
-- {-# LANGUAGE LambdaCase #-}
-- module Zap.IR
--   ( IR(..)
--   , IRBasicBlock(..)
--   , IRError(..)
--   , IRExpr(..)
--   , IRSeq(..)
--   , IRStmt(..)
--   , validateBasicBlock
--   , validateSeq
--   ) where

-- import Data.Array ((!))
-- import Data.Maybe (mapMaybe)
-- import Control.Applicative ((<|>))
-- import Control.Monad (foldM, foldM_, forM_, unless, when)
-- import Control.Monad.Except
-- import qualified Data.Graph as G
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
-- import qualified Data.Text as T
-- import Debug.Trace

-- import Zap.AST

-- data IRNumType
--   = IRInt32
--   | IRInt64
--   | IRFloat32
--   | IRFloat64
--   deriving (Eq, Show, Ord)

-- data IRVecType
--   = IRVec2 IRNumType
--   | IRVec3 IRNumType
--   | IRVec4 IRNumType
--   deriving (Eq, Show, Ord)

-- data IRType
--   = IRTypeNum IRNumType
--   | IRTypeVec IRVecType
--   | IRTypeString
--   | IRTypeBool
--   | IRTypeStruct T.Text [(T.Text, IRType)]
--   | IRTypeArray IRType
--   | IRTypeVoid
--   | IRTypeAny
--   deriving (Eq, Show)

-- data IROp
--   = IRAdd
--   | IRSub
--   | IRMul
--   | IRDiv
--   | IRDot
--   | IRCross
--   | IRLt
--   | IRGt
--   | IREq
--   | IRAddAssign
--   deriving (Show, Eq, Ord)

-- -- | Effects that expressions can have
-- data Effect
--   = ReadEffect      -- Reads from variables
--   | WriteEffect     -- Writes to variables
--   | IOEffect        -- Performs IO
--   | PureEffect      -- Has no effects
--   deriving (Eq, Ord, Show)

-- -- | Expression metadata
-- data IRExprMetadata = IRExprMetadata
--   { exprType :: IRType             -- Type of the expression
--   , metaEffects :: S.Set Effect    -- Effects this expression may have
--   , metaSourcePos :: Maybe (Int, Int)  -- Source line and column if available
--   } deriving (Eq, Show)

-- data IRExprNode
--   = IRConst IRType T.Text        -- Constants (numbers, strings)
--   | IRVar T.Text                 -- Variable references
--   | IRBinOp IROp IRExpr IRExpr  -- Binary operations
--   | IRCall T.Text [IRExpr]      -- Function calls
--   | IRFieldAccess IRExpr T.Text -- Struct field access
--   | IRIndex IRExpr IRExpr       -- Array indexing
--   deriving (Show, Eq)

-- data IRExpr = IRExpr
--   { metadata :: IRExprMetadata
--   , expr :: IRExprNode
--   } deriving (Show, Eq)

-- type Label = T.Text

-- data IR = IR
--   { blocks :: [IRBasicBlock] -- Basic blocks including "main"
--   } deriving (Show, Eq)

-- data IRStmt
--   = IRLabel Label
--   | IRJump Label
--   | IRCondJump IRExpr Label Label
--   | IRExprStmt IRExpr
--   | IRAssign T.Text IRExpr
--   | IRVarDecl T.Text IRType (Maybe IRExpr)
--   | IRBlockParams [(T.Text, IRType)]
--   | IRBreakStmt Label
--   deriving (Show, Eq)

-- data IRBasicBlock = IRBasicBlock
--   { blockName :: T.Text
--   , blockStmts :: [IRStmt]
--   } deriving (Show, Eq)

-- data IRSeq = IRSeq
--   { seqBlocks :: [IRBasicBlock]
--   } deriving (Show, Eq)

-- data IRError
--   -- Function validation errors
--   = TypeMismatch IRType IRType
--   | MissingReturn
--   | InvalidReturn
--   -- Block validation errors
--   | EmptyBasicBlock T.Text
--   | InvalidTerminator T.Text IRStmt
--   -- Seq validation errors
--   | DisconnectedFlow T.Text
--   | UnreachableBlock T.Text
--   | DuplicateLabel T.Text
--   -- Declaration validation errors
--   | UndeclaredVariable T.Text
--   -- Loop validation errors
--   | UnmodifiedLoopVar T.Text
--   deriving (Show, Eq)

-- -- Scope tracking for validation
-- data Scope = Scope
--   { scopeVars :: M.Map T.Text IRType
--   , parentScope :: Maybe Scope
--   , modifiedVars :: S.Set T.Text
--   } deriving (Show, Eq)

-- data ValidationMode = Strict | Permissive
--   deriving (Show, Eq)

-- validateBasicBlockStmt :: Scope -> IRStmt -> Either IRError Scope
-- validateBasicBlockStmt scope = \case
--   IRAssign var expr -> do
--     let exprType' = exprType $ metadata expr
--     case lookupVar var scope of
--       Just varType ->
--         if exprType' /= varType
--           then Left $ TypeMismatch varType exprType'
--           else return scope
--       -- Implicitly add undeclared variables in basic block validation
--       Nothing -> return $ scope { scopeVars = M.insert var exprType' (scopeVars scope) }

--   stmt -> validateStmtWithScope scope stmt

-- validateBasicBlock :: IRBasicBlock -> Either IRError ()
-- validateBasicBlock (IRBasicBlock name stmts) = do
--   when (null stmts) $ Left $ EmptyBasicBlock name

--   -- Validate each statement with scope tracking
--   let initialScope = Scope M.empty Nothing S.empty
--   _ <- foldM validateBasicBlockStmt initialScope stmts

--   -- Validate terminator
--   case lastMaybe stmts of
--     Nothing -> Left $ EmptyBasicBlock name
--     Just stmt -> case stmt of
--       IRJump _ -> Right ()
--       IRCondJump {} -> Right ()
--       IRBreakStmt _ -> Right ()
--       other -> Left $ InvalidTerminator name other

-- -- Helper for safe last element
-- lastMaybe :: [a] -> Maybe a
-- lastMaybe [] = Nothing
-- lastMaybe xs = Just $ last xs

-- -- | Validate a sequence of basic blocks
-- validateSeq :: IRSeq -> Either IRError ()
-- validateSeq (IRSeq blocks) = do
--   -- First validate individual blocks
--   mapM_ (either (error . show) return . validateBasicBlock) blocks

--   -- Build sets of defined labels and jump targets
--   let blockLabels = S.fromList $ map blockName blocks
--       jumpTargets = S.fromList $ concatMap getJumpTargets blocks

--   -- Check for jumps to non-existent blocks
--   case S.toList $ S.difference jumpTargets blockLabels of
--     (missing:_) -> Left $ DisconnectedFlow missing
--     [] -> do
--       -- Check for unreachable blocks
--       case findUnreachableBlocks blocks of
--         (unreachable:_) -> Left $ UnreachableBlock (blockName unreachable)
--         [] -> Right ()

-- -- | Get all jump targets from a block
-- getJumpTargets :: IRBasicBlock -> [T.Text]
-- getJumpTargets block = mapMaybe getTarget (blockStmts block)
--   where
--     getTarget :: IRStmt -> Maybe T.Text
--     getTarget (IRJump target) = Just target
--     getTarget (IRCondJump _ thn els) = Just thn <> Just els
--     getTarget (IRBreakStmt target) = Just target
--     getTarget _ = Nothing

-- -- | Build a directed graph representing control flow
-- buildFlowGraph :: [IRBasicBlock] -> (G.Graph, G.Vertex -> ((), T.Text, [T.Text]), T.Text -> Maybe G.Vertex)
-- buildFlowGraph blocks = G.graphFromEdges edges
--   where
--     edges = [((), name, getJumpTargets block) | block@(IRBasicBlock name _) <- blocks]

-- -- | Find blocks that can't be reached from entry
-- findUnreachableBlocks :: [IRBasicBlock] -> [IRBasicBlock]
-- findUnreachableBlocks blocks =
--   case blocks of
--     [] -> []
--     (entry:_) -> -- Assume first block is entry
--       let (graph, nodeFromVertex, vertexFromKey) = buildFlowGraph blocks
--           entryVertex = case vertexFromKey (blockName entry) of
--                          Just v -> v
--                          Nothing -> error "Entry block not found in graph"
--           reachable = S.fromList $ G.reachable graph entryVertex
--           allVertices = S.fromList [0..length blocks - 1]
--           unreachableVertices = S.difference allVertices reachable
--       in [block | v <- S.toList unreachableVertices,
--                  let (_, name, _) = nodeFromVertex v,
--                  block <- blocks,
--                  blockName block == name]

-- validateSeqWithScope :: Scope -> IRSeq -> Either IRError ()
-- validateSeqWithScope scope (IRSeq blocks) = do
--   -- Validate blocks while tracking scope
--   foldM_ validateBlockWithScope scope blocks
--   where
--     validateBlockWithScope :: Scope -> IRBasicBlock -> Either IRError Scope
--     validateBlockWithScope scope block = do
--       let stmts = blockStmts block
--       foldM validateStmtWithScope scope stmts



-- validateStmtWithScope :: Scope -> IRStmt -> Either IRError Scope
-- validateStmtWithScope scope = do
--   traceM $ "\n=== Validating Statement With Scope ==="
--   \case
--     IRVarDecl name typ mInit -> do
--       traceM $ "\n=== Validating Variable Declaration ==="
--       traceM $ "Declaring: " ++ show name ++ " : " ++ show typ
--       traceM $ "Current scope: " ++ show scope
--       -- Check initialization type matches declaration
--       forM_ mInit $ \init -> do
--         let initType = exprType $ metadata init
--         when (initType /= typ) $
--           Left $ TypeMismatch typ initType

--       -- Add variable to scope
--       return $ scope { scopeVars = M.insert name typ (scopeVars scope) }

--     IRAssign var expr -> do
--       traceM $ "\n=== Validating Assignment Statement ==="
--       let exprType' = exprType $ metadata expr
--       -- If variable is declared, check type matches
--       case lookupVar var scope of
--         Just varType ->
--           if exprType' /= varType
--             then Left $ TypeMismatch varType exprType'
--             else return $ scope { modifiedVars = S.insert var (modifiedVars scope) }
--         -- Reject undeclared variables in function validation
--         Nothing -> Left $ UndeclaredVariable var

--     IRBreakStmt _ -> do
--       traceM "Validating break statement"
--       return scope  -- Break statements don't affect scope

--     _ -> do
--       traceM $ "\n=== Validating Unrecognized Statement ==="
--       traceM $ "Current scope: " ++ show scope
--       return scope

-- -- Helper to collect variable references from expressions
-- collectVarRefs :: IRExpr -> S.Set T.Text
-- collectVarRefs (IRExpr _ node) = case node of
--   IRVar name -> S.singleton name
--   IRBinOp _ e1 e2 -> collectVarRefs e1 `S.union` collectVarRefs e2
--   _ -> S.empty

-- -- Helper for expression scope validation
-- validateExprScope :: Scope -> IRExpr -> Either IRError IRExpr
-- validateExprScope scope expr@(IRExpr meta node) = case node of
--     IRVar name -> do
--         traceM $ "Checking variable " ++ show name ++ " in scope"
--         case M.lookup name (scopeVars scope) of
--             Just varType -> do
--                 -- Return expr with correct type from scope
--                 return $ IRExpr meta { exprType = varType } node
--             Nothing -> throwError $ UndeclaredVariable name

--     IRBinOp op e1 e2 -> do
--         e1' <- validateExprScope scope e1
--         e2' <- validateExprScope scope e2
--         -- Return expr with validated subexpressions
--         return $ IRExpr meta (IRBinOp op e1' e2')

--     _ -> return expr

-- -- Helper to look up variable in scope chain
-- lookupVar :: T.Text -> Scope -> Maybe IRType
-- lookupVar name scope =
--   M.lookup name (scopeVars scope) <|>
--   (parentScope scope >>= lookupVar name)

-- -- New conversion function
-- convertToIR' :: Program -> Either IRError IR
-- convertToIR' (Program tops) = do
--     -- Convert top-level expressions to statements in main block
--     mainStmts <- concat <$> mapM convertTopLevelToStmt tops

--     -- Create main block
--     let mainBlock = IRBasicBlock
--           { blockName = "main"
--           , blockStmts = mainStmts
--           }

--     -- Return IR with just main block for now
--     return $ IR [mainBlock]

-- convertTopLevelToStmt :: TopLevel -> Either IRError [IRStmt]
-- convertTopLevelToStmt (TLExpr e) = convertExprToStmts e
-- convertTopLevelToStmt _ = Right [] -- Ignore other top-level constructs for now

-- convertExprToStmts :: Expr -> Either IRError [IRStmt]
-- convertExprToStmts expr = case expr of
--     -- Convert any expression to a statement by wrapping it
--     expr -> do
--         irExpr <- convertToIRExpr expr
--         return [IRExprStmt irExpr]

-- convertToIRExpr :: Expr -> Either IRError IRExpr
-- convertToIRExpr expr = case expr of
--     StrLit s -> Right $ IRExpr
--         { metadata = IRExprMetadata
--             { exprType = IRTypeString
--             , metaEffects = S.singleton PureEffect
--             , metaSourcePos = Nothing
--             }
--         , expr = IRString (T.pack s)
--         }
--     Call name args -> do
--         -- Convert each argument
--         irArgs <- mapM convertToIRExpr args
--         let effects = if name == "print"
--                      then S.singleton IOEffect
--                      else S.singleton PureEffect
--         Right $ IRExpr
--             { metadata = IRExprMetadata
--                 { exprType = IRTypeVoid  -- print returns void
--                 , metaEffects = effects
--                 , metaSourcePos = Nothing
--                 }
--             , expr = IRCall (T.pack name) irArgs
--             }
--     _ -> Left $ ConversionError "Unsupported expression type"
