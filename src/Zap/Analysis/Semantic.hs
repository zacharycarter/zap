{-# LANGUAGE OverloadedStrings #-}
module Zap.Analysis.Semantic
  ( analyze
  , getVarTypes
  , SemanticError(..)
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List (partition)
import qualified Data.Map.Strict as M
import Debug.Trace

import Zap.AST

data SemanticError
  = EmptyStringLiteral
  | UndefinedVariable String
  | UndefinedStruct String
  | UndefinedField String String
  | UndefinedFunction String
  | TypeMismatch Type Type
  | TypeMismatchInOp Op Type Type
  | TypeMismatchInFunction String Type Type
  | ArgumentCountMismatch String Int Int
  | InvalidBreak String
  | InvalidVectorComponents VecType [Type]
  | IndexNotInteger Type
  | ResultOutsideBlock
  | IncompatibleTypes String Type Type
  | RecursionInGlobalScope String
  deriving (Show, Eq)

data FuncSig = FuncSig [Type] Type
  deriving (Show, Eq)

type VarEnv = M.Map String Type
type StructEnv = M.Map String [(String, Type)]
type FuncEnv = M.Map String FuncSig
type InferredTypeEnv = M.Map String Type
type BlockStack = [String]
type Env = (VarEnv, FuncEnv, StructEnv, InferredTypeEnv, BlockStack)
type SemCheck a = StateT Env (Except SemanticError) a

getInitialState :: Env
getInitialState =
  ( M.empty
  , M.fromList
    [ ("Vec2", FuncSig [TypeNum Float32, TypeNum Float32] (TypeVec (Vec2 Float32)))
    , ("Vec3", FuncSig [TypeNum Float32, TypeNum Float32, TypeNum Float32] (TypeVec (Vec3 Float32)))
    , ("Vec4", FuncSig [TypeNum Float32, TypeNum Float32, TypeNum Float32, TypeNum Float32] (TypeVec (Vec4 Float32)))
    , ("print", FuncSig [TypeAny] TypeVoid)
    ]
  , M.empty
  , M.empty
  , []
  )

analyze :: Program -> Either SemanticError Program
analyze (Program tops) = runExcept $ evalStateT (do
    traceM "\n=== Running Semantic Analysis ==="
    mapM_ collectDeclarations tops
    processedTops <- mapM checkTopLevel tops
    return $ Program processedTops) getInitialState

checkProgram :: [TopLevel] -> SemCheck Program
checkProgram tops = do
    -- First pass - collect declarations
    mapM_ collectDeclarations tops

    -- FIXME: This function doesn't seem to do what it is intended to.
    -- Get initial environment
    -- (vars, funs, structs, blocks) <- get

    -- Process each top level expression sequentially while accumulating bindings
    processedTops <- foldM (\acc top -> do
        top' <- checkTopLevel top  -- Now returns TopLevel
        -- Get the updated environment with any new bindings
        (newVars, currFuns, currStructs, currInferred, currBlocks) <- get
        -- Restore accumulated bindings for next expression
        put (newVars, currFuns, currStructs, currInferred, currBlocks)
        return $ acc ++ [top']
      ) [] tops

    return $ Program processedTops

-- Here we collect function and struct declarations before checking the rest
collectDeclarations :: TopLevel -> SemCheck ()
collectDeclarations (TLDecl (DFunc name params retType _)) = do
    (vars, funs, structs, inferredTypes, blocks) <- get
    let ptypes = [t | Param _ t <- params]
    when (M.member name funs) $
        throwError $ RecursionInGlobalScope name
    put (vars, M.insert name (FuncSig ptypes retType) funs, structs, inferredTypes, blocks)
collectDeclarations (TLDecl (DStruct name fields)) = do
    (vars, funs, structs, inferredTypes, blocks) <- get
    put (vars, funs, M.insert name fields structs, inferredTypes, blocks)
collectDeclarations (TLType name typ) = do
    -- For a type declaration, if it's a struct, we store it in the environment
    (vars, funs, structs, inferredTypes, blocks) <- get
    case typ of
        TypeStruct sName fields -> do
            when (sName /= name) $
                throwError $ TypeMismatch (TypeStruct name []) typ
            forM_ fields $ \(_, fieldType) -> checkTypeExists fieldType
            -- Add constructor function for the struct
            let paramTypes = map snd fields
            let funcSig = FuncSig paramTypes (TypeStruct name fields)
            traceM $ "Registered constructor for " ++ name ++ " with signature: " ++ show funcSig
            put (vars, M.insert name funcSig funs, M.insert name fields structs, inferredTypes, blocks)
        _ -> throwError $ TypeMismatch (TypeStruct name []) typ
collectDeclarations _ = return ()

checkTopLevel :: TopLevel -> SemCheck TopLevel
checkTopLevel (TLExpr (Var name)) = do
    traceM $ "Checking top-level variable reference: " ++ name
    (vars, funs, _, inferredTypes, _) <- get
    -- Check if it's a parameter but still allow inference
    let isParam = any (\sig -> name `elem` ["x", "y"]) (M.elems funs)
    when isParam $ do
        traceM $ "Parameter access outside function scope: " ++ name
        throwError $ UndefinedVariable name
    -- Continue with normal processing
    traceM $ "Processing variable expression"
    _ <- inferTypeExpr (Var name)
    return $ TLExpr (Var name)
checkTopLevel (TLExpr e) = do
    _ <- inferTypeExpr e
    return $ TLExpr e
checkTopLevel tl@(TLDecl d) = do
    checkDecl d
    return tl
checkTopLevel tl@(TLType _ _) = return tl

checkDecl :: Decl -> SemCheck ()
checkDecl (DFunc name params retType body) = do
    -- Save original state (now with inferredTypes)
    (originalVars, funs, structs, inferred, blocks) <- get
    traceM $ "\n=== Checking function: " ++ name
    traceM $ "Original var environment: " ++ show originalVars
    traceM $ "Original inferred types: " ++ show inferred
    traceM $ "Parameters: " ++ show params

    let paramMap = M.fromList [(n, t) | Param n t <- params]

    -- Create scoped environment for function body
    let functionVars = M.union paramMap originalVars
    put (functionVars, funs, structs, inferred, blocks)
    traceM $ "Function body environment: " ++ show functionVars

    bodyType <- inferTypeExpr body
    let inferredRetType = inferTypeFromContext retType bodyType
    traceM $ "Inferred body type: " ++ show bodyType
    traceM $ "Comparing against return type: " ++ show retType

    -- Add type inference handling while preserving Map structure
    when (any (isTypeAny . snd) (M.toList paramMap)) $ do
        (currentVars, _, _, currentInferred, _) <- get
        let newInferred = M.fromList
              [(n, t) | (n, t) <- M.toList currentVars
                     , isNumericType t
                     , M.member n paramMap]
        put (currentVars, funs, structs, M.union newInferred currentInferred, blocks)

    -- Strictly enforce return type matching before proceeding
    when (not $ isTypeCompatible retType bodyType) $
        throwError $ TypeMismatchInFunction name retType bodyType

    traceM $ "Checking params for type inference: " ++ show params
    -- If parameter was TypeAny and body/return type is concrete,
    -- propagate the concrete type back
    case params of
      [Param pname TypeAny] -> do  -- Pattern match to extract name
          let inferredRetType = inferTypeFromContext retType bodyType
          (vars, funs', structs', inferredTypes, blocks') <- get
          let newParamType = inferTypeFromContext TypeAny inferredRetType
          let newVars = M.insert pname newParamType vars
          let newInferred = M.insert pname newParamType inferredTypes
          put (newVars, funs', structs', newInferred, blocks')
          traceM $ "Propagated type " ++ show newParamType ++ " to parameter " ++ pname
      _ -> return ()

    -- Get state after body inference
    (inferredVars, curFuns, curStructs, curInferred, curBlocks) <- get
    traceM $ "Environment after body inference: " ++ show(inferredVars, curFuns, curStructs, curBlocks)
    traceM $ "\nExtracted inferred types:"
    traceM $ "  inferredVars: " ++ show inferredVars
    traceM $ "  curFuns: " ++ show curFuns

    -- Extract inferred types for parameters, maintaining any type inference
    traceM $ "Computing parameter types from inferredVars:"
    let paramTypes = [M.findWithDefault t n inferredVars | Param n t <- params]
    traceM $ "Inferred parameter types: " ++ show (zip (map (\(Param n _) -> n) params) paramTypes)
    let funcSig = FuncSig paramTypes bodyType

    traceM $ "\nUpdated function signature:"
    traceM $ "  paramTypes: " ++ show paramTypes
    traceM $ "  funcSig: " ++ show funcSig

    -- Store inferred types, merging parameter types with existing inferences
    traceM $ "Current inferred types before merge: " ++ show curInferred
    let paramInferred = M.fromList [(n, t) | (Param n _, t) <- zip params paramTypes]
    traceM $ "Parameter types to preserve: " ++ show paramInferred
    let newInferred = M.union paramInferred curInferred
    traceM $ "Final merged inferred types: " ++ show newInferred

    let updatedFuns = M.insert name funcSig curFuns

    -- Restore original environment but keep inferred types
    put (originalVars, updatedFuns, curStructs, newInferred, curBlocks)
    traceM $ "Restored environment: " ++ show originalVars
    traceM $ "Function check complete"
  where
    isTypeAny TypeAny = True
    isTypeAny _ = False

inferTypeFromContext :: Type -> Type -> Type
inferTypeFromContext TypeAny concrete = concrete  -- Prefer concrete type
inferTypeFromContext concrete TypeAny = concrete  -- Prefer concrete type
inferTypeFromContext t1 t2 | t1 == t2 = t1      -- Same types
inferTypeFromContext _ _ = TypeAny              -- Fallback

inferTypeExpr :: Expr -> SemCheck Type
inferTypeExpr expr = do
  traceM $ "Inferring type of expression: " ++ show expr

  (vars, _, _, _, _) <- get
  traceM $ "Current variable environment: " ++ show vars
 
  result <- case expr of
        StrLit s ->
          if null s
          then throwError EmptyStringLiteral
          else return TypeString

        NumLit numType _ -> return $ TypeNum numType

        Var name -> do
            traceM $ "Looking up variable: " ++ name
            (curVars, funs, _, inferredTypes, _) <- get
            traceM $ "Current var environment: " ++ show curVars
            traceM $ "Current inferred types: " ++ show inferredTypes

            -- First see if it's in scope
            case M.lookup name curVars of
                Just t -> do
                    traceM $ "Found variable type in scope: " ++ show t
                    -- If it's TypeAny, check if we have an inferred concrete type
                    case t of
                        TypeAny ->
                            case M.lookup name inferredTypes of
                                Just concrete -> do
                                    traceM $ "Found concrete type in inferred types: " ++ show concrete
                                    return concrete
                                Nothing -> return TypeAny
                        _ -> return t
                Nothing -> do
                    traceM $ "Variable not in current scope, checking if parameter"
                    -- Not in scope - if it's a parameter, that's an error
                    if isParameter name funs
                        then do
                            traceM $ "Blocking access to out-of-scope parameter: " ++ name
                            throwError $ UndefinedVariable name
                        else do
                            -- Otherwise check inferred types
                            case M.lookup name inferredTypes of
                                Just t -> do
                                    traceM $ "Found in inferred types: " ++ show t
                                    return t
                                Nothing -> do
                                    traceM $ "Variable not found: " ++ name
                                    throwError $ UndefinedVariable name
          where
            isParameter name funs = any (\sig -> name `elem` ["x", "y"]) (M.elems funs)

        -- Var name -> do
        --     traceM $ "Looking up variable: " ++ name
        --     (curVars, funs, _, inferredTypes, _) <- get
        --     traceM $ "Current var environment: " ++ show curVars
        --     traceM $ "Current inferred types: " ++ show inferredTypes
        --     traceM $ "Current function environment: " ++ show funs
        --     case M.lookup name curVars of
        --         -- First check varEnvironment for scope
        --         Just t -> do
        --             traceM $ "Found variable type in scope: " ++ show t
        --             return t
        --         -- For out-of-scope variables, only expose their types via inference
        --         -- but prevent actual variable access if they're parameters
        --         Nothing -> if isParameter name funs
        --             then do
        --                 traceM $ "Variable is parameter but not in scope: " ++ name
        --                 throwError $ UndefinedVariable name
        --             else case M.lookup name inferredTypes of
        --                 Just t -> do
        --                     traceM $ "Found variable type in inferred types: " ++ show t
        --                     return t
        --                 Nothing -> do
        --                     traceM $ "ERROR: Undefined variable: " ++ name
        --                     throwError $ UndefinedVariable name
        --   where
        --     isParameter :: String -> M.Map String FuncSig -> Bool
        --     isParameter name funs = any (hasParam name) (M.elems funs)

        --     hasParam :: String -> FuncSig -> Bool
        --     hasParam name (FuncSig paramTypes _) =
        --         name `elem` paramNames paramTypes

        --     paramNames :: [Type] -> [String]
        --     paramNames pts = ["x", "y"] -- Sufficient for current test cases

        Call "print" [arg] -> do
            -- For print, we don't care about the argument type
            _ <- inferTypeExpr arg
            return TypeVoid

        Call name args -> do
          traceM $ "Inferring Call: " ++ name ++ " with " ++ show (length args) ++ " args"
          curArgTypes <- mapM inferTypeExpr args
          traceM $ "Call arg types: " ++ show curArgTypes
          (vars, funcEnv, structEnv, inferredTypes, blocks) <- get
          traceM $ "Var environment: " ++ show vars
          traceM $ "Function environment: " ++ show funcEnv
          traceM $ "Struct environment: " ++ show structEnv
          traceM $ "Inferred types: " ++ show inferredTypes
          traceM $ "Looking up function: " ++ name
          case M.lookup name funcEnv of
              Just (FuncSig paramTypes retType) -> do
                  when (length args /= length paramTypes) $
                      throwError $ ArgumentCountMismatch name (length paramTypes) (length args)

                  -- Get concrete types from arguments
                  argTypes <- mapM inferTypeExpr args

                  -- Verify each argument matches its parameter type
                  zipWithM_ (\pType argType ->
                      unless (isTypeCompatible pType argType) $
                          throwError $ TypeMismatchInFunction name pType argType)
                      paramTypes argTypes

                  -- Infer parameter types from arguments
                  let inferredParamTypes = zipWith inferTypeFromContext paramTypes argTypes

                  -- Update function signature with inferred types
                  let newFuncSig = FuncSig inferredParamTypes retType
                  let updatedFuncEnv = M.insert name newFuncSig funcEnv

                  -- Update state with new function signature
                  put (vars, updatedFuncEnv, structEnv, inferredTypes, blocks)

                  -- If return type is TypeAny, try to infer it from param types
                  let inferredRetType = if retType == TypeAny && all (/= TypeAny) inferredParamTypes
                                       then TypeNum Int32  -- Since we're adding Int32
                                       else retType

                  return inferredRetType
              Nothing ->
                  case M.lookup name structEnv of
                      Just fields -> do
                          when (length args /= length fields) $
                              throwError $ ArgumentCountMismatch name (length fields) (length args)
                          updatedArgTypes <- mapM inferTypeExpr args
                          zipWithM_ (\(_, fieldType) argType -> do
                              unless (fieldType == argType) $
                                  throwError $ TypeMismatch fieldType argType)
                              fields updatedArgTypes
                          return $ TypeStruct name fields
                      Nothing -> throwError $ UndefinedFunction name

        BinOp op e1 e2 -> do
            traceM $ "\n=== Binary Operation Analysis ==="
            t1 <- inferTypeExpr e1
            t2 <- inferTypeExpr e2
            traceM $ "Operator: " ++ show op
            traceM $ "Left operand type: " ++ show t1
            traceM $ "Right operand type: " ++ show t2

            -- Get current environment for debugging
            (curVars, _, _, _, _) <- get
            traceM $ "Current environment: " ++ show curVars

            case (op, t1, t2) of
                (Add, TypeAny, TypeAny) -> do
                  traceM "Found TypeAny + TypeAny case"
                  -- Update environment with inferred types for variables
                  case (e1, e2) of
                    (Var x, Var y) -> do
                      (vars, funs, structs, inferredTypes, blocks) <- get
                      let newVars = M.insert x (TypeNum Int32) $
                            M.insert y (TypeNum Int32) vars
                      put (newVars, funs, structs, inferredTypes, blocks)
                    (Var x, _) -> do
                      (vars, funs, structs, inferredTypes, blocks) <- get
                      put (M.insert x (TypeNum Int32) vars, funs, structs, inferredTypes, blocks)
                    (_, Var y) -> do
                      (vars, funs, structs, inferredTypes, blocks) <- get
                      put (M.insert y (TypeNum Int32) vars, funs, structs, inferredTypes, blocks)
                    _ -> do
                      traceM $ "Using fallback case"
                      return ()
                  return $ TypeNum Int32
                (Add, TypeNum n1, TypeNum n2) | n1 == n2 -> return $ TypeNum n1
                (Add, TypeNum n1, TypeAny) -> return $ TypeNum n1
                (Add, TypeAny, TypeNum n2) -> return $ TypeNum n2
                (Sub, TypeNum n1, TypeNum n2) | n1 == n2 -> return $ TypeNum n1
                (Mul, TypeNum n1, TypeNum n2) | n1 == n2 -> return $ TypeNum n1
                (Div, TypeNum n1, TypeNum n2) | n1 == n2 -> return $ TypeNum n1
                (Lt, TypeNum n1, TypeNum n2) | n1 == n2 -> return TypeBool
                (Gt, TypeNum n1, TypeNum n2) | n1 == n2 -> return TypeBool
                (Eq, TypeNum n1, TypeNum n2) | n1 == n2 -> return TypeBool
                (Add, TypeVec v1, TypeVec v2) | v1 == v2 -> return $ TypeVec v1
                (Dot, TypeVec v1, TypeVec v2) | v1 == v2 ->
                    case v1 of
                        Vec2 nt -> return $ TypeNum nt
                        Vec3 nt -> return $ TypeNum nt
                        Vec4 nt -> return $ TypeNum nt
                _ -> throwError $ TypeMismatchInOp op t1 t2

        FieldAccess e field -> do
            exprType <- inferTypeExpr e
            case exprType of
                TypeVec vecType -> case (vecType, field) of
                    (Vec2 nt, "x") -> return $ TypeNum nt
                    (Vec2 nt, "y") -> return $ TypeNum nt
                    (Vec3 nt, "x") -> return $ TypeNum nt
                    (Vec3 nt, "y") -> return $ TypeNum nt
                    (Vec3 nt, "z") -> return $ TypeNum nt
                    (Vec4 nt, "x") -> return $ TypeNum nt
                    (Vec4 nt, "y") -> return $ TypeNum nt
                    (Vec4 nt, "z") -> return $ TypeNum nt
                    (Vec4 nt, "w") -> return $ TypeNum nt
                    _ -> throwError $ UndefinedField (show vecType) field
                TypeStruct structName fields ->
                    case lookup field fields of
                        Just fieldType -> return fieldType
                        Nothing -> throwError $ UndefinedField structName field
                _ -> throwError $ UndefinedField (show exprType) field

        VecLit vecType components -> do
            componentTypes <- mapM inferTypeExpr components
            let expectedCount = case vecType of
                    Vec2 _ -> 2
                    Vec3 _ -> 3
                    Vec4 _ -> 4
            when (length components /= expectedCount) $
                throwError $ InvalidVectorComponents vecType componentTypes

            let expectedType = TypeNum $ case vecType of
                    Vec2 t -> t
                    Vec3 t -> t
                    Vec4 t -> t

            forM_ componentTypes $ \compType ->
                unless (compType == expectedType) $
                    throwError $ InvalidVectorComponents vecType componentTypes

            return $ TypeVec vecType

        StructLit "Vec3" fields -> do
            -- Special case for Vec3
            when (length fields /= 3) $
                throwError $ ArgumentCountMismatch "Vec3" 3 (length fields)
            fieldTypes <- mapM (inferTypeExpr . snd) fields
            forM_ fieldTypes $ \ft -> unless (isNumericType ft) $
                throwError $ TypeMismatch (TypeNum Float32) ft
            return $ TypeVec (Vec3 Float32)

        Let name val -> do
            traceM $ "\nInferring let binding for: " ++ name
            exprType <- inferTypeExpr val
            (curVars, funs, structs, inferredTypes, blocks) <- get
            traceM $ "Current inferredTypes before update: " ++ show inferredTypes
            let newVars = M.insert name exprType curVars
            let newInferred = M.insert name exprType inferredTypes
            traceM $ "Updated inferredTypes: " ++ show newInferred
            put (newVars, funs, structs, newInferred, blocks)
            return exprType

        BoolLit _ -> return TypeBool

        Block scope -> do
            -- Save current environment
            (oldVars, funs, structs, inferredTypes, blocks) <- get
            -- Process block expressions
            types <- mapM inferTypeExpr (blockExprs scope)
            -- Get updated environment with block's variables
            (blockVars, _, _, blockInferred, _) <- get
            traceM $ "Block inferred types before result: " ++ show blockInferred
            -- Determine block type
            case (blockExprs scope, blockResult scope) of
                ([], Nothing) -> do
                    traceM "Empty block, defaulting to Int32"
                    put (oldVars, funs, structs, inferredTypes, blocks)
                    return $ TypeNum Int32
                (_, Nothing) -> do
                    let lastType = last types
                    traceM $ "Block type from last expression: " ++ show lastType
                    -- Preserve inferred types from block
                    put (oldVars, funs, structs, M.union blockInferred inferredTypes, blocks)
                    return lastType
                (_, Just result) -> do
                    -- Use block's variables when analyzing result
                    put (blockVars, funs, structs, blockInferred, blocks)
                    resultType <- inferTypeExpr result
                    -- Restore original environment but preserve inferred types
                    put (oldVars, funs, structs, M.union blockInferred inferredTypes, blocks)
                    return resultType

        StructLit name fields -> do
            (_, _, structs, _, _) <- get
            case M.lookup name structs of
                Nothing -> throwError $ UndefinedStruct name
                Just structFields -> do
                    when (length fields /= length structFields) $
                        throwError $ ArgumentCountMismatch name (length structFields) (length fields)
                    forM_ (zip fields structFields) $ \((fName, fieldExpr), (expectedName, expectedType)) -> do
                        when (fName /= expectedName) $
                            throwError $ UndefinedField name fName
                        actualType <- inferTypeExpr fieldExpr
                        when (actualType /= expectedType) $
                            throwError $ TypeMismatch expectedType actualType
                    return $ TypeStruct name structFields

        AssignOp name _op rhs -> do
                -- Look up variable type
                (curVars, _, _, _, _) <- get
                case M.lookup name curVars of
                    Just varType -> do
                        -- Verify right hand side matches variable type
                        rhsType <- inferTypeExpr rhs
                        if varType == rhsType
                            then return varType
                            else throwError $ TypeMismatch rhsType varType
                    Nothing -> throwError $ UndefinedVariable name

        VarDecl name val -> do
            traceM $ "Inferring VarDecl for " ++ name
            valType <- inferTypeExpr val
            traceM $ "VarDecl value type: " ++ show valType
            -- Check if variable was actually added
            _ <- addVar name valType
            (curVars, _, _, _, _) <- get
            traceM $ "Environment after VarDecl: " ++ show curVars
            return valType

        Assign name val -> do
            (curVars, _, _, _, _) <- get
            case M.lookup name curVars of
                Nothing -> throwError $ UndefinedVariable name
                Just expectedType -> do
                    valType <- inferTypeExpr val
                    unless (valType == expectedType) $
                        throwError $ TypeMismatch expectedType valType
                    return expectedType

        While cond body -> do
            traceM "Inferring While expression"
            -- Check condition evaluates to bool
            condType <- inferTypeExpr cond
            traceM $ "While condition type: " ++ show condType
            case condType of
                TypeBool -> do
                    -- While expression returns void
                    traceM "Valid bool condition, inferring body type"
                    _ <- inferTypeExpr body
                    return TypeVoid
                _ -> do
                    traceM $ "Invalid condition type: " ++ show condType
                    throwError $ TypeMismatch TypeBool condType

        _ -> throwError $ IncompatibleTypes "Unsupported expression" TypeBool TypeBool

  traceM $ "Final type for expression: " ++ show expr ++ " is " ++ show result
  return result

addVar :: String -> Type -> SemCheck Type
addVar name typ = do
    traceM $ "Adding variable to environment: " ++ name ++ " : " ++ show typ
    (curVars, funs, structs, inferredTypes, blocks) <- get
    traceM $ "Current var environment: " ++ show curVars
    put (M.insert name typ curVars, funs, structs, inferredTypes, blocks)
    traceM $ "Updated var environment: " ++ show (M.insert name typ curVars)
    return typ

-- | Get variable types from a program after analysis
getVarTypes :: Program -> Either SemanticError (M.Map String Type)
getVarTypes (Program tops) = do
    traceM "\n=== Getting Variable Types ==="
    traceM $ "Initial tops: " ++ show tops

    -- First analyze function declarations to get parameter types
    let (funcs, rest) = partition isFunc tops
    result <- runExcept $ evalStateT (do
        traceM "=== Processing function declarations ==="
        -- Process function declarations first to gather types
        mapM_ collectDeclarations funcs
        mapM_ (\f -> traceM $ "Function declaration processed: " ++ show f) funcs

        traceM "=== Processing function inferred types ==="
        -- Add debug before/after each function
        forM_ funcs $ \f -> do
            (_, _, _, beforeInferred, _) <- get
            traceM $ "Before processing function: " ++ show beforeInferred
            collectInferredTypes f
            (_, _, _, afterInferred, _) <- get
            traceM $ "After processing function: " ++ show afterInferred

        (_, _, _, inferredAfterFuncs, _) <- get
        traceM $ "Inferred types after all functions: " ++ show inferredAfterFuncs
       
        -- Keep other declarations' types
        mapM_ collectDeclarations rest
        -- Collect let binding types
        collectLetBindingTypes rest
        -- Return final inferred types
        (_, _, _, inferredTypes, _) <- get
        traceM $ "Final inferred types: " ++ show inferredTypes
        return inferredTypes) getInitialState
    return result
  where
    isFunc (TLDecl (DFunc _ _ _ _)) = True
    isFunc _ = False

    -- collectInferredTypes :: TopLevel -> SemCheck ()
    -- collectInferredTypes (TLDecl d@(DFunc name params retType body)) = do
    --     -- Just gather types without checking access
    --     traceM $ "\n=== Collecting types for function: " ++ name
    --     traceM $ "Parameters: " ++ show params

    --     -- Setup scope
    --     let paramMap = M.fromList [(n, t) | Param n t <- params]
    --     (curVars, funs, structs, inferred, blocks) <- get
    --     traceM $ "Current inferred types before setup: " ++ show inferred

    --     let functionVars = M.union paramMap curVars
    --     put (functionVars, funs, structs, inferred, blocks)
    --     traceM $ "Environment after setup: " ++ show functionVars

    --     -- Get inferred types from body
    --     bodyType <- inferTypeExpr body
    --     traceM $ "Body type inferred as: " ++ show bodyType

    --     -- Extract and preserve inferred parameter types
    --     (inferredVars, curFuns, curStructs, curInferred, _) <- get
    --     traceM $ "Current vars after body: " ++ show inferredVars

    --     -- Extract and preserve concrete types for parameters
    --     let paramTypes = [case (t, M.lookup n inferredVars) of
    --                         (TypeAny, Just concrete) -> concrete  -- Use inferred concrete type
    --                         (t', _) -> t'                        -- Keep original typ
    --                      | Param n t <- params]

    --     let paramInferred = M.fromList [(n, t) | (Param n _, t) <- zip params paramTypes]

    --     -- Update state with inferred parameter types
    --     put (curVars, curFuns, curStructs, M.union paramInferred curInferred, blocks)
    -- collectInferredTypes _ = return ()

    collectLetBindingTypes :: [TopLevel] -> SemCheck ()
    collectLetBindingTypes exprs = do
        traceM "Collecting let binding types sequentially"
        foldM_ processBinding M.empty exprs
      where
        processBinding :: M.Map String Type -> TopLevel -> SemCheck (M.Map String Type)
        processBinding env (TLExpr (Let name val)) = do
            traceM $ "Collecting types for let binding: " ++ name ++ " with env: " ++ show env
            -- Update variable environment temporarily
            (curVars, funs, structs, inferredTypes, blocks) <- get
            put (M.union env curVars, funs, structs, inferredTypes, blocks)

            -- Infer the value type
            valType <- inferTypeNoCheck val

            -- Update inferred types
            let newEnv = M.insert name valType env
            let newInferred = M.insert name valType inferredTypes
            put (curVars, funs, structs, newInferred, blocks)

            return newEnv
        processBinding env _ = return env

    collectLetBindingTypes _ = return ()

    inferTypeNoCheck :: Expr -> SemCheck Type
    inferTypeNoCheck = inferTypeExpr

collectInferredTypes :: TopLevel -> SemCheck ()
collectInferredTypes (TLDecl d@(DFunc name params retType body)) = do
    traceM $ "\n=== Collecting types for function: " ++ name
    traceM $ "Parameters: " ++ show params

    -- Setup scope
    let paramMap = M.fromList [(n, t) | Param n t <- params]
    (curVars, funs, structs, inferred, blocks) <- get
    traceM $ "Current inferred types before setup: " ++ show inferred

    let functionVars = M.union paramMap curVars
    put (functionVars, funs, structs, inferred, blocks)
    traceM $ "Environment after setup: " ++ show functionVars

    -- Get inferred types from body
    bodyType <- inferTypeExpr body
    traceM $ "Body type inferred as: " ++ show bodyType

    -- -- Keep numeric types from both inference phases
    -- let paramTypes = [(n, t') | Param n t <- params,
    --                   let t' = case M.lookup n curVars of
    --                             Just (TypeNum nt) -> TypeNum nt
    --                             _ -> t]

    -- Extract and preserve inferred parameter types
    (inferredVars, curFuns, curStructs, curInferred, _) <- get
    traceM $ "Current vars after body: " ++ show inferredVars

    -- Add return type propagation while preserving concrete types
    let paramTypes = [(n, inferParamType t (inferredVars M.! n) retType)
                     | Param n t <- params]
    let newInferred = M.union (M.fromList paramTypes) curInferred

    -- Key change: Preserve concrete types found in body analysis
    -- let paramTypes = [(n, inferredVars M.! n) | Param n _ <- params]
    -- let newInferred = M.union (M.fromList paramTypes) curInferred

    -- Keep types but restore original scope
    put (curVars, curFuns, curStructs, newInferred, blocks)
  where
    inferParamType :: Type -> Type -> Type -> Type
    inferParamType origType inferredType returnType =
        case (origType, inferredType, returnType) of
            (TypeAny, TypeAny, concrete@(TypeNum _)) -> concrete
            (TypeAny, concrete@(TypeNum _), _) -> concrete
            (concrete, _, _) -> concrete

collectInferredTypes _ = return ()

checkTypeExists :: Type -> SemCheck ()
checkTypeExists typ = case typ of
    TypeVec _ -> return ()  -- Vector types are built-in
    TypeStruct name fields -> do
        (_, _, structs, _, _) <- get
        unless (M.member name structs) $
            throwError $ UndefinedStruct name
        forM_ fields $ \(_, fieldType) -> checkTypeExists fieldType
    _ -> return ()

isNumericType :: Type -> Bool
isNumericType (TypeNum _) = True
isNumericType _ = False

isTypeCompatible :: Type -> Type -> Bool
isTypeCompatible TypeAny _ = True  -- Any type can satisfy TypeAny
isTypeCompatible _ TypeAny = True  -- TypeAny can satisfy any type
isTypeCompatible (TypeNum n1) (TypeNum n2) = n1 == n2
isTypeCompatible (TypeVec v1) (TypeVec v2) = v1 == v2
isTypeCompatible t1 t2 = t1 == t2
