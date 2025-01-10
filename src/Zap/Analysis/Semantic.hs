{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Zap.Analysis.Semantic
  ( analyze
  , analyzeWithSymbols
  , parseSymTable
  , SemanticError(..)
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Debug.Trace

import Zap.AST

data SemanticError
  = EmptyStringLiteral
  | UndefinedVariable String
  | UndefinedStruct String
  | UndefinedField String String
  | UndefinedFunction String
  | DuplicateFieldName String
  | ArgumentCountMismatch String Int Int
  | InvalidBreak String
  | InvalidStruct String
  | RecursionInGlobalScope String
  deriving (Show, Eq)

data FuncSig = FuncSig [Type] Type
  deriving (Show, Eq)

type VarEnv = M.Map String Type
type StructEnv = M.Map String [(String, Type)]
type FuncEnv = M.Map String FuncSig
type BlockStack = [String]
type Env = (VarEnv, FuncEnv, StructEnv, BlockStack, SymbolTable)
type SemCheck a = StateT Env (Except SemanticError) a

initialEnvWithSymbols :: SymbolTable -> Env
initialEnvWithSymbols symTable =
    ( M.empty  -- vars
    , M.fromList  -- funs (keep existing built-ins)
        [ ("Vec2", FuncSig [TypeNum Float32, TypeNum Float32] (TypeVec (Vec2 Float32)))
        , ("Vec3", FuncSig [TypeNum Float32, TypeNum Float32, TypeNum Float32] (TypeVec (Vec3 Float32)))
        , ("Vec4", FuncSig [TypeNum Float32, TypeNum Float32, TypeNum Float32, TypeNum Float32] (TypeVec (Vec4 Float32)))
        , ("print", FuncSig [TypeAny] TypeVoid)
        ]
    , M.empty  -- structs
    , []      -- blocks
    , symTable  -- Pass through the symbol table from parsing
    )

analyze :: Program -> Either SemanticError Program
analyze prog@(Program tops) = case parseSymTable prog of
    Just symTable -> analyzeWithSymbols prog symTable
    Nothing -> Left $ UndefinedStruct "Failed to get symbol table"

analyzeWithSymbols :: Program -> SymbolTable -> Either SemanticError Program
analyzeWithSymbols prog@(Program tops) symTable =
    runExcept $ evalStateT (do
        mapM_ collectTypeDeclarations tops
        mapM_ collectDeclarations tops
        processedTops <- mapM checkWellFormed tops
        return $ Program processedTops) (initialEnvWithSymbols symTable)

parseSymTable :: Program -> Maybe SymbolTable
parseSymTable (Program tops) = Just $ foldr collectStructs emptySymbolTable tops
  where
    collectStructs :: TopLevel -> SymbolTable -> SymbolTable
    collectStructs (TLType name (TypeStruct sid structName)) st =
        -- Add to existing symbol table
        let (_, newSt) = registerStruct name [] st
        in newSt
    collectStructs _ st = st

-- Add this function to handle type declarations
collectTypeDeclarations :: TopLevel -> SemCheck ()
collectTypeDeclarations (TLType name (TypeStruct sid _)) = do
    (vars, funs, structs, blocks, symbols) <- get
    traceM $ "\n=== collectTypeDeclarations ==="
    traceM $ "Processing struct: " ++ name
    traceM $ "Current symbol table: " ++ show symbols
    traceM $ "Current structs map: " ++ show structs

    when (M.member name structs) $
        throwError $ InvalidStruct $ "Duplicate struct definition: " ++ name

    case lookupStruct sid symbols of
        Just def -> do
            traceM $ "Found struct definition: " ++ show def
            let fields = structFields def
            put (vars, funs, M.insert name fields structs, blocks, symbols)
            traceM $ "Updated structs map: " ++ show structs
        Nothing -> do
            traceM $ "Failed to find struct with id: " ++ show sid
            throwError $ UndefinedStruct name
collectTypeDeclarations _ = return ()

collectDeclarations :: TopLevel -> SemCheck ()
collectDeclarations (TLDecl (DFunc name params retType _)) = do
    (vars, funs, structs, blocks, symbols) <- get
    let ptypes = [t | Param _ t <- params]
    when (M.member name funs) $
        throwError $ RecursionInGlobalScope name
    put (vars, M.insert name (FuncSig ptypes retType) funs, structs, blocks, symbols)
collectDeclarations (TLDecl (DStruct name fields)) = do
    (vars, funs, structs, blocks, symbols) <- get
    put (vars, funs, M.insert name fields structs, blocks, symbols)
-- Add this case:
collectDeclarations (TLType name (TypeStruct sid _)) = do
    (vars, funs, structs, blocks, symbols) <- get
    -- Look up struct definition
    case lookupStruct sid symbols of
        Just def -> do
            -- Create function signature with field types as parameters
            let paramTypes = map snd (structFields def)
            -- Register constructor function that returns the struct type
            let retType = TypeStruct sid name
            put (vars, M.insert name (FuncSig paramTypes retType) funs, structs, blocks, symbols)
        Nothing -> throwError $ UndefinedStruct name
collectDeclarations _ = return ()

checkWellFormed :: TopLevel -> SemCheck TopLevel
checkWellFormed tl@(TLExpr expr) = do
    checkExpr expr
    return tl
checkWellFormed tl@(TLDecl d) = do
    checkDecl d
    return tl
checkWellFormed tl = return tl

checkExpr :: Expr -> SemCheck ()
checkExpr = \case
    VarDecl name val -> do
        checkExpr val  -- Check the initialization value
        (vars, funs, structs, blocks, symbols) <- get
        put (M.insert name TypeAny vars, funs, structs, blocks, symbols)
   
    Call name args -> do
        -- Verify function exists and arity matches
        (_, funs, _, _, _) <- get
        case M.lookup name funs of
            Nothing -> throwError $ UndefinedFunction name
            Just (FuncSig params _) -> do
                when (length params /= length args) $
                    throwError $ ArgumentCountMismatch name (length params) (length args)
                mapM_ checkExpr args

    Var name -> do
        -- Check variable is in scope
        (vars, _, _, _, _) <- get
        unless (M.member name vars) $
            throwError $ UndefinedVariable name

    Lit (StringLit s) -> when (null s) $ throwError EmptyStringLiteral

    BinOp _ e1 e2 -> do
        checkExpr e1
        checkExpr e2

    Let name val -> do
        checkExpr val
        -- Need to capture the type of the struct constructor call
        (vars, funs, structs, blocks, symTable) <- get
        let varType = case val of
              Call structName args ->
                -- If it's a struct constructor, use struct type
                case M.lookup structName structs of
                  Just _ -> TypeStruct (fromJust $ M.lookup structName (structNames symTable)) structName
                  Nothing -> TypeAny
              _ -> TypeAny
        -- Store variable with its type
        put (M.insert name varType vars, funs, structs, blocks, symTable)

    FieldAccess expr field -> do
        traceM $ "\n=== checkExpr FieldAccess ==="
        traceM $ "Checking field access: " ++ field
        traceM $ "Base expression: " ++ show expr

        checkExpr expr
        (vars, funs, structs, blocks, symTable) <- get
        traceM $ "Current symbol table: " ++ show symTable
        traceM $ "Current structs map: " ++ show structs

        baseType <- case expr of
            Var name -> do
                traceM $ "Looking up variable type: " ++ name
                case M.lookup name vars of
                    Just t -> return t
                    Nothing -> throwError $ UndefinedVariable name
            _ -> throwError $ UndefinedFunction "Expression cannot be used in field access"

        traceM $ "Base type: " ++ show baseType

        -- Validate struct access
        case baseType of
            TypeStruct sid name -> case lookupStruct sid symTable of
                Just def -> case lookup field (structFields def) of
                    Just _ -> return ()  -- Field exists
                    Nothing -> throwError $ UndefinedField name field
                Nothing -> throwError $ UndefinedStruct name
            _ -> throwError $ InvalidStruct "Field access requires struct type"

    _ -> return ()

checkDecl :: Decl -> SemCheck ()
checkDecl (DFunc _ params _ body) = do
    -- Create new scope with parameters
    (vars, funs, structs, blocks, symbols) <- get
    let paramVars = M.fromList [(name, typ) | Param name typ <- params]
    put (M.union paramVars vars, funs, structs, blocks, symbols)

    -- Check function body
    checkExpr body

    -- Restore original scope
    put (vars, funs, structs, blocks, symbols)

checkDecl (DStruct _ fields) = do
    -- Just verify field names are unique
    let fieldNames = map fst fields
    when (length fieldNames /= length (nub fieldNames)) $
        throwError $ DuplicateFieldName "Duplicate field names in struct"
