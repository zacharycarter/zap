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
import Data.List (isInfixOf, isPrefixOf, nub)
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

        -- NEW: Extract specialized functions from symbol table
        (_, _, _, _, finalSymbols) <- get
        let specializedDecls = map (\(name, def) ->
                TLDecl $ DFunc name
                  [] -- No type params in specialized version
                  (funcParams def)
                  (funcRetType def)
                  (funcBody def))
              $ M.toList
              $ M.filterWithKey (\k _ -> '_' `elem` k)
              $ funcDefs finalSymbols

        return $ Program (processedTops ++ specializedDecls)
        ) (initialEnvWithSymbols symTable)

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
collectDeclarations (TLDecl (DFunc name typeParams params retType body)) = do
    (vars, funs, structs, blocks, symbols) <- get
    let ptypes = [t | Param _ t <- params]
    when (M.member name funs) $
        throwError $ RecursionInGlobalScope name

    let funcDef = FunctionDef name params typeParams retType body
    let newSymbols = symbols { funcDefs = M.insert name funcDef (funcDefs symbols) }

    put (vars, M.insert name (FuncSig ptypes retType) funs, structs, blocks, newSymbols)
collectDeclarations (TLDecl (DStruct name fields)) = do
    (vars, funs, structs, blocks, symbols) <- get
    put (vars, funs, M.insert name fields structs, blocks, symbols)
collectDeclarations (TLType name (TypeStruct sid _)) = do
    (vars, funs, structs, blocks, symbols) <- get
    -- Look up struct definition
    case lookupStruct sid symbols of
        Just def -> do
            -- Register base constructor function
            let paramTypes = map snd (structFields def)
            let retType = TypeStruct sid name
            let baseFuns = M.insert name (FuncSig paramTypes retType) funs

            -- Find and register specialized versions
            let specialized = M.filterWithKey
                              (\k _ -> (name ++ "_") `isPrefixOf` k)
                              (structNames symbols)
            let updatedFuns = M.foldlWithKey'
                              (\fs sname ssid ->
                                case lookupStruct ssid symbols of
                                  Just sDef ->
                                    let sParamTypes = map snd (structFields sDef)
                                    in M.insert sname (FuncSig sParamTypes (TypeStruct ssid sname)) fs
                                  Nothing -> fs)
                              baseFuns
                              specialized

            put (vars, updatedFuns, structs, blocks, symbols)

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
        checkExpr val
        (vars, funs, structs, blocks, symbols) <- get
        put (M.insert name TypeAny vars, funs, structs, blocks, symbols)

    Call name args -> do
        traceM $ "\n=== checkExpr: Call ==="
        traceM $ "Function name: " ++ name
        traceM $ "Arguments: " ++ show args
        (vars, funs, structs, blocks, symbols) <- get

        let baseName = takeWhile (/= '_') name
        traceM $ "Base name: " ++ baseName

        case M.lookup baseName (funcDefs symbols) of
            Just funcDef -> do
                traceM $ "Found generic function: " ++ show funcDef

                -- Extract type args from specialized name
                let typeStr = drop (length baseName + 1) name
                let typeArgs = case typeStr of
                      "i32" -> [TypeNum Int32]
                      "i64" -> [TypeNum Int64]
                      "f32" -> [TypeNum Float32]
                      "f64" -> [TypeNum Float64]
                      _ -> []

                traceM $ "Type arguments: " ++ show typeArgs

                -- Specialize and register in BOTH maps
                case specializeFunctionDef funcDef typeArgs symbols of
                    Right specialized -> do
                        traceM $ "Specialized function: " ++ show specialized

                        -- Register specialized version in funcDefs
                        let newSymbols = symbols
                              { funcDefs = M.insert name specialized (funcDefs symbols) }

                        -- Register function signature
                        let specializedSig = FuncSig
                              [t | Param _ t <- funcParams specialized]
                              (funcRetType specialized)
                        let newFuns = M.insert name specializedSig funs

                        modify $ \s -> (vars, newFuns, structs, blocks, newSymbols)

                        -- Check args after registering
                        when (length (funcParams specialized) /= length args) $
                            throwError $ ArgumentCountMismatch name
                                (length $ funcParams specialized)
                                (length args)
                        mapM_ checkExpr args

                    Left err ->
                        traceM $ "Specialization failed: " ++ err

            Nothing ->
                traceM $ "No generic function found with base name: " ++ baseName

        -- Now check if function exists (either original or just-specialized)
        (_, updatedFuns, _, _, _) <- get  -- Get potentially updated state
        case M.lookup name updatedFuns of
            Nothing -> throwError $ UndefinedFunction name
            Just (FuncSig params _) -> do
                when (length params /= length args) $
                    throwError $ ArgumentCountMismatch name (length params) (length args)
                mapM_ checkExpr args
               
    Var name -> do
        (vars, _, _, _, _) <- get
        unless (M.member name vars) $
            throwError $ UndefinedVariable name

    Lit lit -> case lit of
        StringLit s -> when (null s) $ throwError EmptyStringLiteral
        _ -> return ()  -- Other literals are always valid

    BinOp _ e1 e2 -> do
        checkExpr e1
        checkExpr e2

    Let name val -> do
        checkExpr val
        (vars, funs, structs, blocks, symTable) <- get
        let varType = case val of
              Call structName args ->
                case M.lookup structName (structNames symTable) of
                  Just sid -> TypeStruct sid structName
                  Nothing -> TypeAny
              _ -> TypeAny
        traceM $ "Assigning type " ++ show varType ++ " to variable " ++ name
        put (M.insert name varType vars, funs, structs, blocks, symTable)

    FieldAccess expr field -> do
        traceM $ "\n=== checkExpr FieldAccess ==="
        traceM $ "Checking field access: " ++ field
        traceM $ "Base expression: " ++ show expr

        checkExpr expr  -- First validate the base expression
        (vars, funs, structs, blocks, symTable) <- get

        baseType <- getBaseType expr vars symTable

        case baseType of
            TypeStruct sid name -> case lookupStruct sid symTable of
                Just def -> case lookup field (structFields def) of
                    Just _ -> return ()  -- Field exists
                    Nothing -> throwError $ UndefinedField name field
                Nothing -> throwError $ UndefinedStruct name
            _ -> throwError $ InvalidStruct "Field access requires struct type"

    Break _ _ -> return ()  -- Break statements are checked elsewhere

    Result expr -> checkExpr expr

    If cond thenExpr elseExpr -> do
        checkExpr cond
        checkExpr thenExpr
        checkExpr elseExpr

    While cond body -> do
        checkExpr cond
        checkExpr body

    Block _ exprs mResult -> do
        mapM_ checkExpr exprs
        mapM_ checkExpr mResult

    Assign name expr -> do
        checkExpr expr
        (vars, _, _, _, _) <- get
        unless (M.member name vars) $
            throwError $ UndefinedVariable name

    AssignOp name _ expr -> do
        checkExpr expr
        (vars, _, _, _, _) <- get
        unless (M.member name vars) $
            throwError $ UndefinedVariable name

  where
    getBaseType :: Expr -> VarEnv -> SymbolTable -> SemCheck Type
    getBaseType expr vars symTable = do
        traceM $ "\n=== getBaseType ==="
        traceM $ "Resolving type for: " ++ show expr
        case expr of
            Var name -> case M.lookup name vars of
                Just t -> do
                    traceM $ "Found variable type: " ++ show t
                    return t
                Nothing -> throwError $ UndefinedVariable name

            FieldAccess baseExpr field -> do
                baseType <- getBaseType baseExpr vars symTable
                traceM $ "Base expression type: " ++ show baseType
                case baseType of
                    TypeStruct sid name -> do
                        traceM $ "Looking up struct: " ++ name ++ " (sid: " ++ show sid ++ ")"
                        case lookupStruct sid symTable of
                            Just def -> do
                                traceM $ "Found struct definition: " ++ show def
                                case lookup field (structFields def) of
                                    Just fieldType -> do
                                        -- Handle type specialization for struct fields
                                        let resolvedType = case fieldType of
                                                TypeStruct _ fieldStructName ->
                                                    -- Look up specialized version if it exists
                                                    case name of
                                                        specializedName | "_" `isInfixOf` specializedName ->
                                                            let baseName = takeWhile (/= '_') fieldStructName
                                                                suffix = dropWhile (/= '_') specializedName
                                                                specializedFieldType = baseName ++ suffix
                                                            in case M.lookup specializedFieldType (structNames symTable) of
                                                                Just specializedSid ->
                                                                    TypeStruct specializedSid specializedFieldType
                                                                Nothing -> fieldType
                                                        _ -> fieldType
                                                _ -> fieldType
                                        traceM $ "Resolved field type: " ++ show resolvedType
                                        return resolvedType
                                    Nothing -> throwError $ UndefinedField name field
                            Nothing -> throwError $ UndefinedStruct name
                    _ -> throwError $ InvalidStruct "Field access requires struct type"

            _ -> throwError $ InvalidStruct "Invalid base expression for field access"

checkDecl :: Decl -> SemCheck ()
checkDecl (DFunc _ _typeParams params _ body) = do
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
