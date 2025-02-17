{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zap.Analysis.Semantic
  ( analyze,
    analyzeWithSymbols,
    parseSymTable,
    initialEnv,
    Environment (..),
    SemanticError (..),
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Char as C
import Data.List (isPrefixOf, nub, unfoldr)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace
import Zap.AST

-- | Keep existing error types but add type errors
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
  | TypeError Type Type String -- New: Type errors with messages
  | InfiniteType String Type -- New: Occurs check failures
  deriving (Show, Eq)

-- | Type constraints with source locations for error reporting
data Constraint
  = CEquality Type Type String
  | CTruthable Type String
  | CBreakable Type
  deriving (Show, Eq)

-- | Analysis environment with phases
data Environment = Environment
  { envSymbols :: SymbolTable, -- All type info lives here
    envBlocks :: [String], -- For break/continue validation
    envNextVar :: Int, -- Fresh type variables
    envConstraints :: [Constraint], -- Collected constraints
    envBreakType :: Maybe Type,
    envCurrentFunction :: Maybe String
  }
  deriving (Show)

type Substitution = M.Map String Type

class Substitutable a where
  applySubstTo :: Substitution -> a -> a

instance Substitutable Type where
  applySubstTo = applySubst

instance Substitutable TopLevel where
  applySubstTo subst = \case
    TLExpr expr -> TLExpr (applySubstTo subst expr)
    TLDecl decl -> TLDecl (applySubstTo subst decl)
    tl -> tl -- Type declarations don't get substituted

instance Substitutable Expr where
  applySubstTo subst = \case
    Let name val -> Let name (applySubstTo subst val)
    Call name args ->
      let appliedArgs = map (applySubstTo subst) args
       in case args of
            [arg]
              | isFnameStructConstructor name && not (isSpecialized name) ->
                  let argType = case arg of
                        Lit (IntLit _ (Just t)) -> TypeNum t
                        Lit (FloatLit _ (Just t)) -> TypeNum t
                        _ -> TypeNum Int64
                   in Call (name ++ "_" ++ typeToSuffix argType) appliedArgs
            _ -> Call name appliedArgs
    Var name -> Var name -- Variables don't get substituted
    Lit lit -> Lit lit -- Literals don't get substituted
    BinOp op e1 e2 -> BinOp op (applySubstTo subst e1) (applySubstTo subst e2)
    VarDecl name val -> VarDecl name (applySubstTo subst val)
    Assign name val -> Assign name (applySubstTo subst val)
    AssignOp name op val -> AssignOp name op (applySubstTo subst val)
    Block name exprs result ->
      Block
        name
        (map (applySubstTo subst) exprs)
        (fmap (applySubstTo subst) result)
    Break label expr -> Break label (fmap (applySubstTo subst) expr)
    Result expr -> Result (applySubstTo subst expr)
    If cond thenExpr elseExpr ->
      If
        (applySubstTo subst cond)
        (applySubstTo subst thenExpr)
        (applySubstTo subst elseExpr)
    StructLit name fields ->
      StructLit
        name
        [(f, applySubstTo subst e) | (f, e) <- fields]
    FieldAccess expr field -> FieldAccess (applySubstTo subst expr) field
    ArrayLit typ exprs ->
      ArrayLit
        (applySubstTo subst typ)
        (map (applySubstTo subst) exprs)
    Index arr idx -> Index (applySubstTo subst arr) (applySubstTo subst idx)
    While cond body -> While (applySubstTo subst cond) (applySubstTo subst body)
    Case scrutinee patterns ->
      Case
        (applySubstTo subst scrutinee)
        [(pat, applySubstTo subst expr) | (pat, expr) <- patterns]

instance Substitutable Decl where
  applySubstTo subst (DFunc name params typeParams retType body) =
    DFunc
      name
      params
      typeParams
      (applySubstTo subst retType)
      (applySubstTo subst body)
  applySubstTo _ decl = decl

instance Substitutable Pattern where
  applySubstTo subst = \case
    PConstructor name pats -> PConstructor name (map (applySubstTo subst) pats)
    PLiteral lit -> PLiteral lit -- Literals don't get substituted
    PVar name -> PVar name -- Variable patterns don't get substituted
    PWildcard -> PWildcard -- Wildcards don't get substituted

type SemCheck a = StateT Environment (Except SemanticError) a

-- | Helper to create initial environment
initialEnv :: SymbolTable -> Environment
initialEnv symTable =
  Environment
    { envSymbols = registerOptionConstructors symTable, -- Add option constructors
      envBlocks = [],
      envNextVar = 0,
      envConstraints = [],
      envBreakType = Nothing,
      envCurrentFunction = Nothing
    }

parseSymTable :: Program -> Maybe SymbolTable
parseSymTable (Program tops) = Just $ foldr collectStructs emptySymbolTable tops
  where
    collectStructs :: TopLevel -> SymbolTable -> SymbolTable
    collectStructs (TLType name (TypeStruct _sid _)) st =
      let (_, newSt) = registerStruct name [] st
       in newSt
    collectStructs _ st = st

-- | Main analysis entry points
analyze :: Program -> Either SemanticError Program
analyze prog@(Program tops) = case parseSymTable prog of
  Just symTable -> fmap fst $ analyzeWithSymbols prog symTable
  Nothing -> Left $ UndefinedStruct "Failed to get symbol table"

analyzeWithSymbols :: Program -> SymbolTable -> Either SemanticError (Program, SymbolTable)
analyzeWithSymbols prog@(Program tops) symTable =
  runExcept $ do
    (prog', finalState) <- runStateT (analyzeProgramPhases prog) (initialEnv symTable)
    return (prog', envSymbols finalState)

-- | Phased program semantic analysis
analyzeProgramPhases :: Program -> SemCheck Program
analyzeProgramPhases (Program tops) = do
  traceM "\n=== Starting Program Analysis ==="
  traceM $ "Initial tops: " ++ show tops

  -- First step: Register all basic declarations
  collectedTops <- mapM collectBasicDeclarations tops
  st1 <- gets envSymbols
  traceM $ "After basic declarations:"
  traceM $ "Function definitions: " ++ show (M.keys $ funcDefs st1)
  traceM $ "Struct definitions: " ++ show (M.keys $ structNames st1)

  -- Second step: Register specialized versions
  traceM $ "Before specializations:" ++ show st1
  st2 <- registerAllSpecializations st1 collectedTops
  verifySpecializedStructs st2
  modify $ \s -> s {envSymbols = st2}
  st3 <- gets envSymbols
  traceM $ "After specializations: " ++ show st3
  traceM $ "Function definitions: " ++ show (M.keys $ funcDefs st3)
  traceM $ "Struct definitions: " ++ show (M.keys $ structNames st3)

  -- \**New: Immediately update function definitions with specialized types**
  modify $ \s -> s {envSymbols = updateAllFunctionDefs st3}
  st4 <- gets envSymbols
  traceM $ "Symbol table after updateAllFunctionDefs: " ++ show st4

  -- Convert both specialized structs and functions back to AST
  traceM $ "Converting specializations back to AST"

  -- Find specialized functions
  let specFuncs = [name | (name, _) <- M.toList (funcDefs st4), isSpecialized name]
  traceM $ "Found specialized function names: " ++ show specFuncs

  -- Find specialized structs
  let specStructs =
        [ (name, sid) | (name, sid) <- M.toList (structNames st4), isSpecialized name
        ]
  traceM $ "Found specialized struct names: " ++ show specStructs

  -- Generate AST nodes for both
  let specTops =
        -- First add struct type declarations
        [TLType name (TypeStruct sid name) | (name, sid) <- specStructs]
          -- Then add function declarations using existing helper
          ++ [ TLDecl $ specializedFuncDefToAST name def
               | (name, def) <- M.toList (funcDefs st4),
                 isSpecialized name
             ]

  traceM $ "Generated specialized tops: " ++ show specTops

  traceM $ "Sym table before collecting type constraints: " ++ show st4

  -- Third step: Collect type constraints
  constrainedTops <- mapM collectConstraints (tops ++ specTops) -- Include specializations
  constraints <- gets envConstraints
  traceM $ "Collected constraints: " ++ show constraints

  st5 <- gets envSymbols

  traceM $ "Sym table after collecting type constraints: " ++ show st5

  subst <- solveConstraints constraints
  traceM $ "Solved substitution: " ++ show subst

  st6 <- gets envSymbols

  traceM $ "Sym table after solving constraints: " ++ show st6

  let finalTops = map (applySubstTo subst) constrainedTops
  traceM $ "Final tops before final func update: " ++ show finalTops
  -- Update all function definitions using our updateFuncDef helper.
  finalSyms <- gets envSymbols
  traceM $ "Final symbol table after final func update: " ++ show finalSyms
  return $ Program finalTops

-- | Basic declaration registration (first phase)
collectBasicDeclarations :: TopLevel -> SemCheck TopLevel
collectBasicDeclarations tl = do
  st <- gets envSymbols
  _ <- case tl of
    TLDecl (DFunc name typeParams params retType body) -> do
      traceM $ "\n=== collectBasicDeclarations: Function ==="
      traceM $ "Registering function: " ++ name
      traceM $ "Parameters before specialization: " ++ show params
      traceM $ "Struct definitions: " ++ show (M.toList $ structDefs st)
      traceM $ "Struct names: " ++ show (M.toList $ structNames st)

      -- Specialize parameter types
      let specializedParams =
            map
              ( \(Param pname ptyp) ->
                  Param pname (updateTypeWithSpecialization ptyp st)
              )
              params

      traceM $ "Parameters after specialization: " ++ show specializedParams

      let funcDef =
            FunctionDef
              { funcName = name,
                funcTypeParams = typeParams,
                funcParams = specializedParams, -- Use specialized params
                funcRetType = retType,
                funcBody = body
              }

      modify $ \s ->
        s {envSymbols = st {funcDefs = M.insert name funcDef (funcDefs st)}}
      return st
    TLType name (TypeStruct sid _) -> do
      traceM $ "\n=== collectBasicDeclarations: Struct ==="
      traceM $ "Registering struct type: " ++ name

      case lookupStruct sid st of
        Just def -> do
          -- Register constructor function
          let fields = structFields def
          let fieldParams =
                [Param f t | (_, (f, t)) <- zip [1 :: Int ..] fields]
          let funcDef =
                FunctionDef
                  { funcName = name,
                    funcTypeParams = structParams def,
                    funcParams = fieldParams,
                    funcRetType = TypeStruct sid name,
                    funcBody =
                      let litFields = [(f, Var f) | (f, _) <- fields]
                          structExpr = StructLit name litFields
                       in Block name [] (Just structExpr)
                  }

          modify $ \s ->
            s
              { envSymbols =
                  st {funcDefs = M.insert name funcDef (funcDefs st)}
              }
          return st
        Nothing -> throwError $ UndefinedStruct name
    _ -> return st

  return tl

-- | Specialization declaration registration (second phase)
registerAllSpecializations ::
  SymbolTable ->
  [TopLevel] ->
  StateT Environment (Except SemanticError) SymbolTable
registerAllSpecializations st tops = do
  traceM "\n=== registerAllSpecializations ==="

  let decls = getDecls tops
  -- Find base "generic" symbols: either a function w/ type params, or a generic struct
  let baseSymbols =
        nub $
          [ name
            | DFunc name typeParams _ _ _ <- decls,
              not (null typeParams)
          ]
            ++ [ name
                 | TLType name (TypeStruct _ _) <- tops
               ]
  traceM $ "Base generic symbols: " ++ show baseSymbols

  -- Gather implicit specializations from constructor calls
  let implicitSpecs = concatMap findImplicitSpecializations tops
  traceM $ "Found implicit specializations: " ++ show implicitSpecs

  -- Example of collecting *all* calls, if you want to debug:
  let allCalls = concatMap findCalls tops
  traceM $ "All calls (recursively): " ++ show allCalls

  -- Fold over each "base symbol" (like "Pair", "Box", etc.) registering specializations
  foldM (registerSpecForBase implicitSpecs) st baseSymbols
  where
    -- Helper to extract function declarations
    getDecls :: [TopLevel] -> [Decl]
    getDecls = map extractDecl . filter isFuncDecl
      where
        extractDecl :: TopLevel -> Decl
        extractDecl (TLDecl d) = d
        extractDecl (TLExpr _) = error "isFuncDecl should prevent TLExpr"
        extractDecl (TLType _ _) = error "isFuncDecl should prevent TLType"

        isFuncDecl (TLDecl (DFunc {})) = True
        isFuncDecl _ = False

    -- Helper to find calls anywhere inside top-level expressions
    findCalls :: TopLevel -> [(String, [Expr])]
    findCalls (TLExpr e) = findCallsInExpr e
    findCalls _ = []

    findCallsInExpr :: Expr -> [(String, [Expr])]
    findCallsInExpr = \case
      Call name args -> (name, args) : concatMap findCallsInExpr args
      Let _ val -> findCallsInExpr val
      Block _ es mbE -> concatMap findCallsInExpr es ++ maybe [] findCallsInExpr mbE
      If c t e -> findCallsInExpr c ++ findCallsInExpr t ++ findCallsInExpr e
      While c b -> findCallsInExpr c ++ findCallsInExpr b
      StructLit _ fs -> concatMap (findCallsInExpr . snd) fs
      FieldAccess b _ -> findCallsInExpr b
      ArrayLit _ es -> concatMap findCallsInExpr es
      Index a i -> findCallsInExpr a ++ findCallsInExpr i
      Assign _ rhs -> findCallsInExpr rhs
      AssignOp _ _ r -> findCallsInExpr r
      VarDecl _ rhs -> findCallsInExpr rhs
      Break _ mbE -> maybe [] findCallsInExpr mbE
      Result r -> findCallsInExpr r
      _ -> [] -- Lit, Var, etc.

    --------------------------------------------------------------------------------
    --  findImplicitSpecializations:
    --    Gathers (baseName, [Type]) from constructor calls like Pair_i64_i32(...)
    --------------------------------------------------------------------------------
    findImplicitSpecializations :: TopLevel -> [(String, [Type])]
    findImplicitSpecializations (TLExpr expr) = do
      traceM "\n=== findImplicitSpecializations (TLExpr) ==="
      traceM $ "Expression: " ++ show expr
      findImplicitSpecsInExpr expr
    findImplicitSpecializations _ = []

    -- Helper to recursively find specs in expressions
    findImplicitSpecsInExpr :: Expr -> [(String, [Type])]
    findImplicitSpecsInExpr expr = do
      traceM "\n=== findImplicitSpecsInExpr ==="
      traceM $ "Processing expression: " ++ show expr

      case expr of
        -- Handle direct specialized function call like id_i32(...)
        Call name args -> do
          traceM $ "Found call: " ++ name
          case break (== '_') name of
            (base, '_' : spec) -> do
              traceM $ "Found specialized call: base=" ++ base ++ ", spec=" ++ spec
              let types = case spec of
                    "i32" -> [TypeNum Int32]
                    "i64" -> [TypeNum Int64]
                    "f32" -> [TypeNum Float32]
                    "f64" -> [TypeNum Float64]
                    _ -> [] -- Keep empty list as fallback
              traceM $ "Parsed types: " ++ show types
              [(base, types)] ++ concatMap findImplicitSpecsInExpr args
            _ -> concatMap findImplicitSpecsInExpr args

        -- Recurse into other expression types
        Let _ val -> findImplicitSpecsInExpr val
        Block _ exprs mresult ->
          concatMap findImplicitSpecsInExpr exprs ++ maybe [] findImplicitSpecsInExpr mresult
        If cond t f ->
          concatMap findImplicitSpecsInExpr [cond, t, f]
        While cond body ->
          findImplicitSpecsInExpr cond ++ findImplicitSpecsInExpr body
        BinOp _ e1 e2 ->
          concatMap findImplicitSpecsInExpr [e1, e2]
        _ -> []

    -- If your parser has already turned "Pair[i64, i32]" into "Pair_i64_i32",
    -- then this function reverts to "Pair". Otherwise, you could no-op it.
    stripSuffixIfAny :: String -> String
    stripSuffixIfAny n =
      case break (== '_') n of
        (base, "") -> base -- no underscores => "Pair"
        (base, _) -> base -- "Pair_i64_i32" -> "Pair"

    --------------------------------------------------------------------------------
    -- The actual fold function: for each baseName, register all discovered param-lists
    --------------------------------------------------------------------------------
    registerSpecForBase ::
      [(String, [Type])] ->
      SymbolTable ->
      String ->
      StateT Environment (Except SemanticError) SymbolTable
    registerSpecForBase implicitSpecs st' baseName = do
      traceM $ "\n=== registerSpecForBase: " ++ baseName
      traceM $ "Current symbol table state:"
      traceM $ "- Implicit specs: " ++ show implicitSpecs
      traceM $ "- Struct defs: " ++ show (M.keys $ structDefs st')
      traceM $ "- Function defs: " ++ show (M.keys $ funcDefs st')
      traceM $ "- Base constructor function: " ++ show (M.lookup baseName (funcDefs st'))

      -- First handle struct path if applicable
      stWithStructs <- case M.lookup baseName (structNames st') of
        Just sid -> case lookupStruct sid st' of
          Just def -> do
            traceM $ "Found base struct: " ++ show def
            let specializedVersions =
                  [ parseTypeArgs (drop (length baseName + 1) name)
                    | (name, _) <- M.toList (structNames st'),
                      name /= baseName,
                      stripSuffixIfAny name == baseName
                  ]
            traceM $ "Found specialized versions from names: " ++ show specializedVersions

            -- Find all implied specializations for this base type
            let thisBaseSpecs = [(ts, nm) | (nm, ts) <- implicitSpecs, nm == baseName]
            traceM $ "Specs for " ++ baseName ++ ": " ++ show thisBaseSpecs

            -- Also look for nested uses in other types' fields
            let nestedSpecs = findNestedSpecializations baseName st'
            traceM $ "Found nested uses: " ++ show nestedSpecs

            -- Combine all specializations
            let allSpecs = nub $ specializedVersions ++ map fst thisBaseSpecs ++ nestedSpecs
            traceM $ "All struct specializations to create: " ++ show allSpecs

            foldM (registerSpecialization def baseName) st' allSpecs
          Nothing -> return st'
        Nothing -> return st'

      -- Then handle function specializations if applicable
      case M.lookup baseName (funcDefs stWithStructs) of
        Just baseFuncDef | not (null (funcTypeParams baseFuncDef)) -> do
          traceM $ "Found generic function: " ++ show baseFuncDef
          let thisBaseSpecs = [(ts, nm) | (nm, ts) <- implicitSpecs, nm == baseName, not (null ts)]
          traceM $ "Function specializations needed: " ++ show thisBaseSpecs

          foldM (\s (types, name) -> registerSpecializedFunc baseFuncDef s (name ++ "_" ++ concatMap typeToSuffix types, [])) stWithStructs thisBaseSpecs
        _ -> return stWithStructs
      where
        -- \| Find places where a type is used in other structs' fields
        findNestedSpecializations :: String -> SymbolTable -> [[Type]]
        findNestedSpecializations baseName' st'' = do
          traceM $ "\n=== findNestedSpecializations for " ++ baseName' ++ " ==="
          let specs = [paramType | StructDef {structFields = fields} <- M.elems (structDefs st''), (_, fieldType) <- fields, paramType <- extractTypeParams baseName' fieldType]
          traceM $ "Found nested usages: " ++ show specs
          return specs

        -- \| Extract type parameters when a type is used in a field
        extractTypeParams :: String -> Type -> [Type]
        extractTypeParams baseName' = \case
          TypeStruct _ name
            | name == baseName' ->
                -- If this is a parameter field like Box[T], grab T from context
                case name of
                  n
                    | '_' `elem` n ->
                        -- Already specialized, extract concrete type
                        parseTypeArgs $ dropWhile (/= '_') n
                  _ -> []
            | takeWhile (/= '_') name == baseName' ->
                -- Found specialized version like Box_i32
                parseTypeArgs $ dropWhile (/= '_') name
          TypeParam param ->
            -- Found a type parameter usage like Box[T]
            [TypeParam param]
          _ -> []

        registerSpecialization ::
          StructDef ->
          String ->
          SymbolTable ->
          [Type] ->
          StateT Environment (Except SemanticError) SymbolTable
        registerSpecialization baseDef baseNm st'' paramTypes = do
          traceM $ "\n=== registerSpecialization ==="
          traceM $ "Base name: " ++ show baseNm
          traceM $ "Base struct: " ++ show baseDef
          traceM $ "Param types: " ++ show paramTypes
          traceM $ "Current state: " ++ show st''

          if null paramTypes || any isTypeParam paramTypes
            then do
              traceM "Skipping specialization with type parameters"
              return st''
            else do
              let specName = getMultiParamName baseNm paramTypes
              traceM $ "  specialized struct name: " ++ specName

              -- 1) Register the specialized struct:
              let (newSid, stWithStruct) = registerSpecializedStruct specName baseDef paramTypes st''
              traceM $ "  registerSpecializedStruct done, new struct id = " ++ show newSid

              -- 2) Now specialize the constructor function.
              case M.lookup baseNm (funcDefs stWithStruct) of
                Just baseFuncDef -> do
                  traceM $ "  Found base constructor func: " ++ baseNm
                  traceM $ "  specializing function with paramTypes = " ++ show paramTypes
                  case specializeFunctionDef baseFuncDef paramTypes stWithStruct of
                    Right specFuncDef -> do
                      let specFuncDef' =
                            specFuncDef
                              { funcParams =
                                  map
                                    (\(Param n t) -> Param n (updateTypeWithSpecialization t stWithStruct))
                                    (funcParams specFuncDef),
                                funcRetType = updateTypeWithSpecialization (funcRetType specFuncDef) stWithStruct
                              }
                      traceM $ "  function specialization success => " ++ specName
                      let updatedFuncs = M.insert specName specFuncDef' (funcDefs stWithStruct)
                      let finalSt = stWithStruct {funcDefs = updatedFuncs}
                      traceM $ "  updated symbol table with specialized func => " ++ specName
                      return finalSt
                    Left errMsg -> do
                      traceM $ "  function specialization error => " ++ errMsg
                      -- Skip adding the specialized function if it fails
                      return stWithStruct
                Nothing -> do
                  traceM $ "  no base constructor func found for " ++ baseNm
                  return stWithStruct
          where
            isTypeParam :: Type -> Bool
            isTypeParam (TypeParam _) = True
            isTypeParam _ = False

registerSpecializedFunc :: FunctionDef -> SymbolTable -> (String, [Expr]) -> SemCheck SymbolTable
registerSpecializedFunc baseDef st (specName, _) = do
  traceM $ "\n=== registerSpecializedFunc ==="
  traceM $ "Creating specialized version " ++ specName
  traceM $ "From base definition: " ++ show baseDef

  -- Extract type from specialized name
  let paramType = case drop 1 $ dropWhile (/= '_') specName of
        "i32" -> TypeNum Int32
        "i64" -> TypeNum Int64
        "f32" -> TypeNum Float32
        "f64" -> TypeNum Float64
        _ -> TypeAny

  -- Create specialized function
  case specializeFunctionDef baseDef [paramType] st of
    Right specDef -> do
      traceM $ "Created specialized definition: " ++ show specDef
      let updatedDef =
            specDef
              { funcParams =
                  map
                    (\(Param n t) -> Param n (updateTypeWithSpecialization t st))
                    (funcParams specDef),
                funcRetType = updateTypeWithSpecialization (funcRetType specDef) st
              }
      return $ st {funcDefs = M.insert specName updatedDef (funcDefs st)}
    Left err -> do
      traceM $ "Failed to create specialized definition: " ++ show err
      return st

verifySpecializedStructs :: SymbolTable -> StateT Environment (Except SemanticError) ()
verifySpecializedStructs st = do
  let specialized = M.filter (not . null . structParams) (structDefs st)
  forM_ (M.toList specialized) $ \(_, def) -> do
    let baseName = structName def
    forM_ (M.keys $ structNames st) $ \name ->
      when (baseName `isPrefixOf` name && name /= baseName) $ do
        -- Verify specialized version exists and has correct fields
        case M.lookup name (structNames st) of
          Nothing -> lift $ throwError $ UndefinedStruct name
          Just specSid -> case lookupStruct specSid st of
            Nothing -> lift $ throwError $ UndefinedStruct name
            Just specDef ->
              when (null $ structParams specDef) $ do
                -- Verify all fields are present and specialized
                forM_ (structFields specDef) $ \(fname, _) ->
                  when (not $ any ((== fname) . fst) (structFields def)) $ do
                    lift $ throwError $ UndefinedField name fname

-- | Helper to parse type arguments from specialized name suffix
parseTypeArgs :: String -> [Type]
parseTypeArgs s =
  let components =
        filter (not . null) $
          map (takeWhile (/= '_')) $
            unfoldr
              ( \str ->
                  if null str
                    then Nothing
                    else Just (str, drop 1 $ dropWhile (/= '_') str)
              )
              s
   in map parseTypeArg components
  where
    parseTypeArg :: String -> Type
    parseTypeArg "i32" = TypeNum Int32
    parseTypeArg "i64" = TypeNum Int64
    parseTypeArg "f32" = TypeNum Float32
    parseTypeArg "f64" = TypeNum Float64
    parseTypeArg _ = TypeAny

-- | Constraint collection (third phase)
collectConstraints :: TopLevel -> SemCheck TopLevel
collectConstraints = \case
  TLExpr expr -> do
    (_, expr') <- inferExpr expr
    return $ TLExpr expr'
  TLDecl (DFunc name typeParams params retType body) -> do
    -- Save old function name
    oldFn <- gets envCurrentFunction
    -- Set current function
    modify $ \s -> s {envCurrentFunction = Just name}

    -- Check for duplicate definition
    defined <- isDefined name
    when defined $
      throwError $
        RecursionInGlobalScope name

    -- Collect constraints from body
    (bodyType, body') <- withNewScope $ do
      mapM_ bindParam params
      inferExpr body

    -- Add constraint for return type
    addConstraint
      ( CEquality bodyType retType $
          "Return type of function " ++ name
      )

    modify $ \s -> s {envCurrentFunction = oldFn}
    return $ TLDecl $ DFunc name typeParams params retType body'
  other -> return other -- Pass through type declarations

-- | Core type inference
inferExpr :: Expr -> SemCheck (Type, Expr)
inferExpr e = do
  traceM $ "\n=== inferExpr : " ++ show e ++ " ==="
  case e of
    Var name -> do
      typ <- lookupVar name
      return (typ, Var name)
    Lit lit -> return (literalType lit, Lit lit)
    Let name val -> do
      (valType, val') <- inferExpr val
      traceM $ "\n=== Let binding ==="
      traceM $ "Binding " ++ name ++ " with type " ++ show valType

      -- Check declared type if one exists
      st <- gets envSymbols
      case lookupVarType name st of
        Just declaredType -> do
          traceM $ "Found declared type: " ++ show declaredType
          addConstraint
            ( CEquality declaredType valType $
                "Type mismatch in let binding - expected "
                  ++ show declaredType
                  ++ " but got "
                  ++ show valType
            )
        Nothing -> return ()

      -- Store final type
      let finalType = case val of
            Call structName' _ ->
              case M.lookup structName' (structNames st) of
                Just sid -> TypeStruct sid structName'
                Nothing -> valType
            _ -> valType

      traceM $ "Final type for binding: " ++ show finalType
      bindVar name finalType
      return (finalType, Let name val')
    FieldAccess expr field -> do
      traceM $ "\n=== FieldAccess ==="
      traceM $ "Base expression: " ++ show expr
      traceM $ "Accessing field: " ++ field

      -- First get base expression's type
      st <- gets envSymbols
      baseType <- getBaseType expr st
      (_, expr') <- inferExpr expr
      traceM $ "Base expression type: " ++ show baseType

      case baseType of
        TypeStruct sid name -> do
          traceM $ "Looking up struct " ++ name ++ " with sid " ++ show sid
          traceM $ "Available struct definitions: " ++ show (M.toList $ structDefs st)
          case lookupStruct sid st of
            Just def -> do
              traceM $ "Found struct definition: " ++ show def
              traceM $ "Available fields: " ++ show (structFields def)
              case lookup field (structFields def) of
                Just fieldType -> do
                  traceM $ "Found field type: " ++ show fieldType
                  return (fieldType, FieldAccess expr' field)
                Nothing -> throwError $ UndefinedField name field
            Nothing -> throwError $ UndefinedStruct name
        _ ->
          throwError $
            TypeError
              baseType
              TypeAny
              ("Expected struct type for field access, got " ++ show baseType)
    VarDecl name val -> do
      (valType, val') <- inferExpr val
      bindVar name valType -- Register variable type
      return (valType, VarDecl name val')
    Assign name val -> do
      -- Look up variable's type
      varType <- lookupVar name
      (valType, val') <- inferExpr val
      -- Value must match variable's type
      addConstraint
        ( CEquality valType varType $
            "Assignment to variable " ++ name
        )
      return (valType, Assign name val')
    AssignOp name op val -> do
      -- Similar to regular assignment but ensure numeric
      varType <- lookupVar name
      when (not $ isNumericType varType) $
        throwError $
          TypeError
            varType
            (TypeNum Int64)
            "Compound assignment requires numeric type"

      (valType, val') <- inferExpr val
      when (not $ isNumericType valType) $
        throwError $
          TypeError
            valType
            (TypeNum Int64)
            "Compound assignment value must be numeric"

      return (varType, AssignOp name op val')
    BinOp op e1 e2 -> do
      (t1, e1') <- inferExpr e1
      (t2, e2') <- inferExpr e2

      case op of
        -- Comparison operators can work on numbers but produce numeric "boolean" results
        Lt | isNumericType t1 && isNumericType t2 -> do
          let resultType = TypeNum Int64 -- Comparison returns 0 or 1
          return (resultType, BinOp op e1' e2')
        Gt | isNumericType t1 && isNumericType t2 -> do
          let resultType = TypeNum Int64
          return (resultType, BinOp op e1' e2')
        Add | isNumericType t1 && isNumericType t2 -> do
          let resultType = TypeNum (maxNumType (numTypeOf t1) (numTypeOf t2))
          return (resultType, BinOp op e1' e2')
        _ | isNumericType t1 && isNumericType t2 -> do
          let resultType = TypeNum (maxNumType (numTypeOf t1) (numTypeOf t2))
          return (resultType, BinOp op e1' e2')
        _ ->
          throwError $
            TypeError t1 t2 $
              "Binary operator " ++ show op ++ " requires numeric operands"
    Call "print" [arg] -> do
      traceM $ "\n=== inferExpr: print call ==="
      traceM $ "Print argument: " ++ show arg
      (argType, arg') <- inferExpr arg
      traceM $ "Argument type after inference: " ++ show argType
      st <- gets envSymbols
      traceM $ "Current symbol table: " ++ show st

      let isPrintable = isPrintableType argType (Just st)
      traceM $ "isPrintableType returned: " ++ show isPrintable

      when (not isPrintable) $ do
        traceM $ "Type check failed - throwing TypeError"
        throwError $
          TypeError
            argType
            TypeAny
            "Print argument must be a printable type"

      traceM $ "Print type check passed"
      return (TypeVoid, Call "print" [arg'])
    Call "Some" [arg] -> do
      traceM $ "\n=== inferExpr: Some call ==="
      traceM $ "Analyzing Some constructor with argument: " ++ show arg
      (argType, arg') <- inferExpr arg
      traceM $ "Argument evaluated to type: " ++ show argType
      traceM $ "Looking up available option types in symbol table"
      st <- gets envSymbols
      traceM $ "Current symbol table structs: " ++ show (M.keys $ structDefs st)
      traceM $ "Current symbol table names: " ++ show (M.keys $ structNames st)
      case argType of
        TypeStruct _ name -> do
          let optionStructName = "__option_" ++ name
          case M.lookup optionStructName (structNames st) of
            Just optionSid -> do
              let result = (TypeOption (TypeStruct optionSid optionStructName), Call ("Some_" ++ name) [arg'])
              traceM $ "Result: " ++ show result
              return result
            Nothing -> throwError $ UndefinedStruct optionStructName
        _ -> do
          let result = (TypeOption argType, Call "Some" [arg'])
          traceM $ "Result: " ++ show result
          return result
    Call fname@('N' : 'o' : 'n' : 'e' : '_' : typeSuffix) [] -> do
      traceM $ "\n=== Processing None call ==="
      traceM $ "Function name: " ++ fname
      traceM $ "Type suffix: " ++ typeSuffix

      st <- gets envSymbols

      paramType <- case typeSuffix of
        "i32" -> return $ TypeNum Int32
        "i64" -> return $ TypeNum Int64
        "f32" -> return $ TypeNum Float32
        "f64" -> return $ TypeNum Float64
        structName -> do
          -- First look up the generic type for the given struct name.
          genericType <- case M.lookup structName (structNames st) of
            Just sid -> return $ TypeStruct sid structName
            Nothing -> throwError $ UndefinedStruct structName
          -- Now update it with any available specialization.
          let specializedType = updateTypeWithSpecialization genericType st
          return specializedType

      traceM $ "Inferred parameter type: " ++ show paramType
      return (TypeOption paramType, Call fname [])
    Call name args -> do
      traceM $ "\n=== inferExpr: Call ==="
      traceM $ "Name: " ++ name
      traceM $ "Args: " ++ show args
      syms <- gets envSymbols
      traceM $ "Current symbol table: " ++ show syms
      traceM $ "Looking up function definition in symbol table: " ++ name
      case M.lookup name (funcDefs syms) of
        Just def -> do
          traceM $ "Found function definition in symbol table: " ++ show def
          -- Check argument count matches
          when (length args /= length (funcParams def)) $
            throwError $
              ArgumentCountMismatch
                name
                (length (funcParams def))
                (length args)
          -- Infer types of arguments
          (argTypes, args') <- unzip <$> mapM inferExpr args
          -- Update the function’s parameter types and return type using updateTypeWithSpecialization
          let updatedParams = map (\(Param n t) -> Param n (updateTypeWithSpecialization t syms)) (funcParams def)
              paramTypes = [t | Param _ t <- updatedParams]
              newRetType = updateTypeWithSpecialization (funcRetType def) syms
          -- Add constraints using the updated parameter types
          zipWithM_
            ( \paramType argType ->
                addConstraint (CEquality paramType argType ("Argument to " ++ name))
            )
            paramTypes
            argTypes
          let result = (newRetType, Call name args')
          traceM $ "Result: " ++ show result
          return result
        Nothing -> throwError $ UndefinedFunction name
    While cond body -> do
      (condType, cond') <- inferExpr cond
      if isTruthable condType
        then do
          -- Add constraint that condition type must be compatible with boolean context
          addConstraint (CTruthable condType "While condition must be convertible to boolean")

          -- Keep constraint checking for body
          (_, body') <- inferExpr body

          -- Body can be any type since it's executed for side effects
          return (TypeVoid, While cond' body')
        else do
          throwError $
            TypeError
              condType
              TypeAny
              "While condition must be a truthable type"
    If cond thenExpr elseExpr -> do
      traceM $ "\n=== inferExpr: If ==="
      (condType, cond') <- inferExpr cond
      traceM $ "Condition type: " ++ show condType

      if isTruthable condType
        then do
          addConstraint (CTruthable condType "If condition must be convertible to boolean")

          -- Save current break type
          oldBreak <- gets envBreakType
          traceM $ "Saved break type before then: " ++ show oldBreak

          -- Infer then branch
          (thenType, then') <- inferExpr thenExpr
          thenBreak <- gets envBreakType
          traceM $ "Then branch type: " ++ show thenType
          traceM $ "Then break type: " ++ show thenBreak

          -- Reset break type for else branch
          modify $ \env -> env {envBreakType = oldBreak}

          -- Infer else branch
          (elseType, else') <- inferExpr elseExpr
          elseBreak <- gets envBreakType
          traceM $ "Else branch type: " ++ show elseType
          traceM $ "Else break type: " ++ show elseBreak

          -- NEW: Get enclosing function return type
          st <- gets envSymbols
          let fnRetType = case M.lookup "divOrZero" (funcDefs st) of
                Just def -> Just $ funcRetType def
                Nothing -> Nothing
          traceM $ "Enclosing function type: " ++ show fnRetType

          -- Use break type OR function return type for result
          let resultType = case (thenBreak, elseBreak, fnRetType) of
                (Just bt, _, _) -> bt
                (_, Just bt, _) -> bt
                (_, _, Just ft) -> ft -- NEW: Use function type if no breaks
                _ -> thenType

          traceM $ "Selected result type: " ++ show resultType

          -- Restore original break state
          modify $ \env -> env {envBreakType = oldBreak}

          -- Add constraint using result type
          addConstraint
            ( CEquality
                elseType
                resultType
                "Then and else branches must have same type"
            )

          return (resultType, If cond' then' else')
        else do
          throwError $ TypeError condType TypeAny "If condition must be a truthable type"
    Block name exprs mresult -> do
      traceM $ "\n=== inferExpr: Block ==="
      traceM $ "Block name: " ++ name
      traceM $ "Expressions: " ++ show exprs

      -- Get enclosing function type
      st <- gets envSymbols
      let fnType = case M.lookup name (funcDefs st) of
            Just def -> Just $ funcRetType def
            Nothing -> Nothing
      traceM $ "Enclosing function type: " ++ show fnType

      -- Save current break type
      oldBreakType <- gets envBreakType
      traceM $ "Saved break type: " ++ show oldBreakType

      -- Process expressions
      (types, exprs') <- unzip <$> mapM inferExpr exprs

      -- Get current break type after processing expressions
      currentBreakType <- gets envBreakType

      -- Get block type, now considering break type
      blockType <- case (mresult, types, currentBreakType) of
        (Just result, _, _) -> do
          (resultType, _) <- inferExpr result
          return resultType
        (Nothing, t : ts, _) -> do
          return $ last (t : ts)
        (Nothing, [], Just bt) -> do
          -- Empty block with break - use break type
          return bt
        (Nothing, [], Nothing) -> do
          -- Empty block without break - try function type
          curFn <- gets envCurrentFunction
          st' <- gets envSymbols
          case curFn >>= \fn -> M.lookup fn (funcDefs st') of
            Just def -> return $ funcRetType def
            Nothing -> return TypeVoid

      -- Restore previous break type before returning
      modify $ \env -> env {envBreakType = oldBreakType}

      traceM $ "Inferred block type: " ++ show blockType
      return (blockType, Block name exprs' mresult)
    Break (Just label) mexpr -> do
      -- Same lookup logic as before
      fnRetType <- case label of
        fname | isFunctionLabel fname -> do
          st <- gets envSymbols
          case M.lookup fname (funcDefs st) of
            Just def -> return $ funcRetType def
            Nothing -> throwError $ UndefinedFunction fname
        _ -> throwError $ InvalidBreak label

      -- Store break type in environment before processing expression
      modify $ \env -> env {envBreakType = Just fnRetType}

      -- Rest remains the same
      case mexpr of
        Just expr -> do
          (exprType, expr') <- inferExpr expr
          addConstraint
            ( CEquality
                exprType
                fnRetType
                ("Break expression in " ++ label)
            )
          return (fnRetType, Break (Just label) (Just expr'))
        Nothing -> return (fnRetType, Break (Just label) Nothing)
    Break Nothing Nothing -> do
      traceM "\n=== inferExpr: Unlabeled break ==="
      -- For unlabeled breaks in while loops, we return TypeVoid
      -- since they're used for control flow only
      return (TypeVoid, Break Nothing Nothing)
    Break Nothing (Just expr) -> do
      -- Get enclosing function's return type
      fnName <- gets envCurrentFunction
      case fnName of
        Just name -> do
          st <- gets envSymbols
          case M.lookup name (funcDefs st) of
            Just def -> do
              (exprType, expr') <- inferExpr expr
              -- Break value must match function return type
              addConstraint
                ( CEquality exprType (funcRetType def) $
                    "Break value type in " ++ name
                )
              return (funcRetType def, Break Nothing (Just expr'))
            Nothing -> throwError $ UndefinedFunction name
        Nothing -> throwError $ InvalidBreak "break with value outside function"
    StructLit name fields -> do
      traceM $ "Inferring struct literal: " ++ name
      syms <- gets envSymbols
      case M.lookup name (structNames syms) of
        Just sid -> case lookupStruct sid syms of
          Just def -> do
            -- Check and infer field types
            inferredFields <- forM fields $ \(fname, fexpr) -> do
              case lookup fname (structFields def) of
                Just expectedType -> do
                  (actualType, fexpr') <- inferExpr fexpr
                  addConstraint
                    ( CEquality
                        actualType
                        expectedType
                        ("Field " ++ fname ++ " in struct " ++ name)
                    )
                  return (fname, fexpr')
                Nothing -> throwError $ UndefinedField name fname
            return (TypeStruct sid name, StructLit name inferredFields)
          Nothing -> throwError $ UndefinedStruct name
        Nothing -> throwError $ UndefinedStruct name
    Result expr -> do
      (exprType, expr') <- inferExpr expr
      return (exprType, Result expr')
    ArrayLit elemType elems -> do
      (elemTypes, elems') <- unzip <$> mapM inferExpr elems
      -- All elements should match the declared element type
      forM_ elemTypes $ \typ ->
        addConstraint
          ( CEquality typ elemType $
              "Array element type mismatch"
          )
      return (TypeArray elemType, ArrayLit elemType elems')
    Index arr idx -> do
      (arrType, arr') <- inferExpr arr
      (idxType, idx') <- inferExpr idx
      -- Index must be numeric
      addConstraint
        ( CEquality
            idxType
            (TypeNum Int32)
            "Array index must be numeric"
        )
      case arrType of
        TypeArray elemType ->
          return (elemType, Index arr' idx')
        _ ->
          throwError $
            TypeError
              arrType
              (TypeArray TypeAny)
              "Cannot index non-array type"
    Case scrutinee patterns -> do
      traceM "\n=== inferExpr: Case expression ==="
      -- First infer type of scrutinee
      (scrutType, scrutExpr) <- inferExpr scrutinee
      traceM $ "Scrutinee type: " ++ show scrutType

      -- Infer types of all patterns and their expressions
      typedPatterns <- forM patterns $ \(pat, expr) -> do
        traceM $ "\n=== Checking pattern: " ++ show pat
        -- Enter new scope for pattern variables
        (patType, boundVars) <- inferPattern pat scrutType
        traceM $ "Pattern type: " ++ show patType
        traceM $ "Bound variables: " ++ show boundVars

        -- Add pattern bindings to scope
        withNewScope $ do
          forM_ boundVars $ \(name, typ) ->
            bindVar name typ

          -- Infer expression with pattern bindings
          (exprType, exprExpr) <- inferExpr expr
          traceM $ "Pattern clause expr type: " ++ show exprType

          return (pat, exprExpr, exprType)

      -- All expressions must have same type
      let exprTypes = [t | (_, _, t) <- typedPatterns]
      case exprTypes of
        [] -> throwError $ TypeError scrutType TypeVoid "Empty case expression"
        (t : ts) -> do
          forM_ ts $ \t' ->
            addConstraint (CEquality t t' "All case branches must return same type")

          return (t, Case scrutExpr [(p, e) | (p, e, _) <- typedPatterns])
  where
    getBaseType :: Expr -> SymbolTable -> SemCheck Type
    getBaseType expr symTable = do
      traceM $ "\n=== getBaseType ==="
      traceM $ "Resolving type for: " ++ show expr
      case expr of
        Var name -> case lookupVarType name symTable of
          Just t -> do
            traceM $ "Found variable type: " ++ show t
            return t
          Nothing -> throwError $ UndefinedVariable name
        FieldAccess baseExpr field -> do
          baseType <- getBaseType baseExpr symTable
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
                                specializedName
                                  | "_" `T.isInfixOf` (T.pack specializedName) ->
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

    inferPattern :: Pattern -> Type -> SemCheck (Type, [(String, Type)])
    inferPattern pat scrutType = do
      traceM $ "\n=== inferPattern ==="
      traceM $ "Pattern: " ++ show pat
      traceM $ "Scrutinee type: " ++ show scrutType

      case pat of
        PConstructor name args -> do
          traceM $ "Checking constructor pattern: " ++ name

          -- For Option types
          case name of
            "Some" -> case scrutType of
              TypeOption innerType -> do
                -- Infer argument patterns
                boundVars <-
                  concat
                    <$> forM
                      args
                      ( \argPat -> do
                          (argType, argVars) <- inferPattern argPat innerType
                          addConstraint (CEquality argType innerType "Constructor argument type")
                          return argVars
                      )
                return (scrutType, boundVars)
              _ -> throwError $ TypeError scrutType (TypeOption TypeAny) "Expected Option type"
            "None" -> case scrutType of
              TypeOption _ -> return (scrutType, [])
              _ -> throwError $ TypeError scrutType (TypeOption TypeAny) "Expected Option type"
            _ -> throwError $ TypeError scrutType TypeAny ("Unknown constructor: " ++ name)
        PLiteral lit -> do
          -- Add this case to handle literals
          traceM $ "Literal pattern: " ++ show lit
          let litType = case lit of
                IntLit _ (Just t) -> TypeNum t
                IntLit _ Nothing -> TypeNum Int32 -- Default to Int32
                FloatLit _ (Just t) -> TypeNum t
                FloatLit _ Nothing -> TypeNum Float32
                StringLit _ -> TypeString
                BooleanLit _ -> TypeBool

          -- Add constraint that literal type matches scrutinee type
          addConstraint (CEquality litType scrutType "Literal pattern type match")
          return (litType, []) -- Literals don't bind variables
        PVar name -> do
          traceM $ "Variable pattern: " ++ name
          return (scrutType, [(name, scrutType)])
        PWildcard -> do
          traceM "Wildcard pattern"
          return (scrutType, []) -- Wildcards don't bind variables

-- | Constraint solving
solveConstraints :: [Constraint] -> SemCheck Substitution
solveConstraints constraints = do
  traceM $ "\n=== solveConstraints ==="
  traceM $ "Solving constraints: " ++ show constraints
  foldM solveConstraint emptySubst constraints
  where
    solveConstraint :: Substitution -> Constraint -> SemCheck Substitution
    solveConstraint subst constraint = do
      traceM $ "\n=== solveConstraint ==="
      traceM $ "Current substitution: " ++ show subst
      traceM $ "Processing constraint: " ++ show constraint

      result <- case constraint of
        CEquality t1 t2 src -> do
          traceM $ "Unifying types: " ++ show t1 ++ " = " ++ show t2
          traceM $ "Source: " ++ src
          lift $ do
            s <- unifyTypes t1 t2 src False -- No implicit conversions in equality constraints
            let composed = composeSubst s subst
            traceM $ "Unification result: " ++ show composed
            return composed
        CTruthable t src -> do
          traceM $ "Checking truthable constraint: " ++ show t
          case t of
            TypeBool -> return subst
            TypeNum _ -> return subst
            TypeParam _ -> do
              u <- lift $ unifyTruthable t src
              return $ composeSubst u subst
            _ -> throwError $ TypeError t TypeBool "Type is not truthable"
        CBreakable _ -> return subst

      traceM $ "Result: " ++ show result
      return result

-- | Type unification
unifyTypes :: Type -> Type -> String -> Bool -> Except SemanticError Substitution
unifyTypes t1 t2 src allowImplicit = do
  traceM $ "\n=== unifyTypes ==="
  traceM $ "t1: " ++ show t1
  traceM $ "t2: " ++ show t2
  traceM $ "source: " ++ show src
  traceM $ "allow implicit: " ++ show allowImplicit

  let result = case (t1, t2) of
        _ | t1 == t2 -> return emptySubst
        (TypeParam name, _) ->
          if occursCheck name t2
            then throwError $ TypeError t1 t2 ("Infinite type in " ++ src)
            else return $ singleSubst name t2
        (_, TypeParam _) -> unifyTypes t2 t1 src allowImplicit
        (TypeNum n1, TypeNum n2) ->
          if n1 == n2 || (allowImplicit && isImplicitlyConvertible n1 n2)
            then return emptySubst
            else throwError $ TypeError t1 t2 ("Numeric type mismatch in " ++ src)
        -- Handle option type unification
        (TypeOption inner1, TypeOption inner2) -> do
          traceM $ "Unifying option types - inner1: " ++ show inner1 ++ ", inner2: " ++ show inner2
          unifyTypes inner1 inner2 src allowImplicit

        -- Handle struct type unification considering specialization
        (TypeStruct sid1 name1, TypeStruct sid2 name2) -> do
          let base1 = stripSpecialization name1
              base2 = stripSpecialization name2
          traceM $ "Unifying structs: " ++ base1 ++ " vs " ++ base2
          -- Allow specialized type to unify with its base type if params match
          if base1 == base2
            then return emptySubst
            else throwError $ TypeError t1 t2 ("Struct type mismatch in " ++ src)
        _ -> throwError $ TypeError t1 t2 ("Type mismatch in " ++ src)

  traceM $ "Unification result: " ++ show result
  result

-- Helper to strip type specialization suffix
stripSpecialization :: String -> String
stripSpecialization name
  | "__option_" `isPrefixOf` name =
      let rest = drop (length ("__option_" :: String)) name
       in case break (== '_') rest of
            (base, _) -> base
  | otherwise =
      case break (== '_') name of
        (base, _) -> base

-- | Type unification for truthable types
unifyTruthable :: Type -> String -> Except SemanticError Substitution
unifyTruthable (TypeParam name) _ =
  if occursCheck name (TypeNum Int64)
    then throwError $ InfiniteType name (TypeNum Int64)
    else
      if occursCheck name TypeBool
        then throwError $ InfiniteType name TypeBool
        else return $ M.insert name (TypeNum Int64) emptySubst
unifyTruthable t src
  | t == TypeBool || isNumericType t = return emptySubst
  | otherwise = throwError $ TypeError t TypeBool ("Type " ++ show t ++ " is not truthable in " ++ src)

-- | Helper functions
isImplicitlyConvertible :: NumType -> NumType -> Bool
isImplicitlyConvertible from to = case (from, to) of
  (Int32, Int64) -> True -- Allow widening from i32 to i64
  (Float32, Float64) -> True -- Allow widening from f32 to f64
  (Int32, Float32) -> True -- Allow int32 to float32 (safe)
  (Int32, Float64) -> True -- Allow int32 to float64 (safe)
  (Int64, Float64) -> True -- Allow int64 to float64 (safe)
  (from', to') -> from' == to' -- Only allow exact matches otherwise
  _ -> False

literalType :: Literal -> Type
literalType = \case
  IntLit _ (Just t) -> TypeNum t
  IntLit _ Nothing -> TypeNum Int64 -- Default to 64-bit
  FloatLit _ (Just t) -> TypeNum t
  FloatLit _ Nothing -> TypeNum Float64
  StringLit _ -> TypeString
  BooleanLit _ -> TypeBool

-- Determine widest numeric type
maxNumType :: NumType -> NumType -> NumType
maxNumType n1 n2 = case (n1, n2) of
  (Float64, _) -> Float64
  (_, Float64) -> Float64
  (Float32, _) -> Float32
  (_, Float32) -> Float32
  (Int64, _) -> Int64
  (_, Int64) -> Int64
  _ -> Int32

numTypeOf :: Type -> NumType
numTypeOf = \case
  TypeNum t -> t -- Extract numeric type directly
  -- Add default case for type checking/inference
  _ -> Int64 -- Default to widest integer type

isDefined :: String -> SemCheck Bool
isDefined name = do
  syms <- gets envSymbols
  return $ case lookupVarType name syms of
    Just _ -> True
    Nothing -> False

isNumericType :: Type -> Bool
isNumericType (TypeNum _) = True
isNumericType _ = False

isTruthable :: Type -> Bool
isTruthable = \case
  TypeNum _ -> True -- All numbers (0 = false, non-0 = true)
  TypeBool -> True -- Boolean values
  _ -> False -- Can extend for other types later

isPrintableType :: Type -> Maybe SymbolTable -> Bool
isPrintableType t mbSt =
  trace ("\n=== isPrintableType ===\nChecking type: " ++ show t) $
    case t of
      TypeNum n ->
        trace ("TypeNum " ++ show n ++ " -> True") True
      TypeString ->
        trace "TypeString -> True" True
      TypeBool ->
        trace "TypeBool -> True" True
      TypeParam p ->
        trace ("TypeParam " ++ show p ++ "\nSymbol table: " ++ show mbSt) $
          case mbSt of
            Just st ->
              trace ("Looking up var type for: " ++ p) $
                case lookupVarType p st of
                  Just specType ->
                    trace ("Found specialized type: " ++ show specType) $
                      isPrintableType specType (Just st)
                  Nothing ->
                    trace "No specialization found" False
            Nothing ->
              trace "No symbol table provided" False
      other ->
        trace ("Other type: " ++ show other ++ " -> False") False

addConstraint :: Constraint -> SemCheck ()
addConstraint constraint = do
  traceM $ "\n=== addConstraint ==="
  traceM $ "Adding constraint: " ++ show constraint
  modify $ \s -> s {envConstraints = constraint : envConstraints s}

bindVar :: String -> Type -> SemCheck ()
bindVar name typ = modify $ \s ->
  s {envSymbols = registerVarType name typ (envSymbols s)}

lookupVar :: String -> SemCheck Type
lookupVar name = do
  st <- gets envSymbols
  case lookupVarType name st of
    Just t -> return t
    Nothing -> throwError $ UndefinedVariable name

bindParam :: Param -> SemCheck ()
bindParam (Param name typ) = do
  st <- gets envSymbols
  let specializedType = case lookupVarType name st of
        Just newType -> newType
        Nothing -> typ
  bindVar name specializedType

withNewScope :: SemCheck a -> SemCheck a
withNewScope m = do
  oldState <- get
  result <- m
  modify $ \s -> s {envSymbols = envSymbols oldState}
  return result

emptySubst :: Substitution
emptySubst = M.empty

singleSubst :: String -> Type -> Substitution
singleSubst var typ = M.singleton var typ

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = M.map (applySubst s1) s2 `M.union` s1

applySubst :: Substitution -> Type -> Type
applySubst subst = \case
  TypeParam name -> M.findWithDefault (TypeParam name) name subst
  TypeArray inner -> TypeArray (applySubst subst inner)
  TypeStruct sid name -> TypeStruct sid name -- Structs don't get substituted
  t -> t

occursCheck :: String -> Type -> Bool
occursCheck var = \case
  TypeParam name -> name == var
  TypeArray inner -> occursCheck var inner
  _ -> False

-- Helper to identify specialized functions
isSpecialized :: String -> Bool
isSpecialized name = any (`T.isSuffixOf` T.pack name) ["_i32", "_i64", "_f32", "_f64"]

-- Convert function definition back to AST
specializedFuncDefToAST :: String -> FunctionDef -> Decl
specializedFuncDefToAST name def =
  DFunc
    name
    [] -- No type params in specialized version
    (funcParams def)
    (funcRetType def)
    (funcBody def)

-- | Returns True if the given string begins with the option prefix.
isOptionPrefix :: String -> Bool
isOptionPrefix s = "__option_" `T.isPrefixOf` T.pack s

-- | Given a type and the current symbol table, if the type is a struct (or
-- an option over a struct) and a specialized version exists, return the specialized type.
updateTypeWithSpecialization :: Type -> SymbolTable -> Type
updateTypeWithSpecialization t syms = do
  let _ = trace ("\n=== updateTypeWithSpecialization ===") ()
  let _ = trace ("Input type: " ++ show t) ()
  let _ = trace ("Available structs: " ++ show (M.keys $ structDefs syms)) ()
  let _ = trace ("Available names: " ++ show (M.keys $ structNames syms)) ()

  case t of
    TypeOption (TypeStruct sid name) -> do
      let base = stripSpecialization name
      let _ = trace ("Found option of struct, base name: " ++ base) ()
      -- First look for a specialized option type candidate.
      let candidateOption =
            [ (n, specSid)
              | (n, specSid) <- M.toList (structNames syms),
                isOptionPrefix n,
                let stripped = drop (length ("__option_" :: String)) n,
                stripped == base || stripSpecialization stripped == base
            ]
      let _ = trace ("Option candidates: " ++ show candidateOption) ()
      -- Otherwise look for a specialized version of the underlying type.
      let candidateGeneric =
            [ (n, specSid)
              | (n, specSid) <- M.toList (structNames syms),
                not (isOptionPrefix n),
                stripSpecialization n == base,
                n /= base
            ]
      let _ = trace ("Generic candidates: " ++ show candidateGeneric) ()

      case candidateOption of
        ((specName, specSid) : _) -> do
          let result = TypeOption (TypeStruct specSid specName)
          trace ("Using specialized option type: " ++ show result) result
        [] -> case candidateGeneric of
          ((specName, specSid) : _) -> do
            let result = TypeOption (TypeStruct specSid specName)
            trace ("Using generic specialized type: " ++ show result) result
          [] -> TypeOption (TypeStruct sid name)
    TypeOption inner -> do
      let _ = trace ("Found nested option type") ()
      -- Recursively update inner type
      let updated = updateTypeWithSpecialization inner syms
      let result = TypeOption updated
      trace ("Updated nested option type: " ++ show result) result
    TypeStruct sid name -> do
      let base = stripSpecialization name
      let _ = trace ("Found struct, base name: " ++ base) ()
      let candidate =
            [ (n, specSid)
              | (n, specSid) <- M.toList (structNames syms),
                not (isOptionPrefix n),
                stripSpecialization n == base,
                n /= base
            ]
      let _ = trace ("Struct candidates: " ++ show candidate) ()
      case candidate of
        ((specName, specSid) : _) -> do
          let result = TypeStruct specSid specName
          trace ("Using specialized struct: " ++ show result) result
        [] -> TypeStruct sid name
    _ -> do
      trace ("No specialization needed for type: " ++ show t) t

-- | Update a function definition’s parameter and return types using updateTypeWithSpecialization.
updateFuncDef :: FunctionDef -> SymbolTable -> FunctionDef
updateFuncDef fd syms = do
  let _ = trace ("\n=== updateFuncDef ===") ()
  let _ = trace ("Function: " ++ funcName fd) ()
  let _ = trace ("Original params: " ++ show (funcParams fd)) ()
  let _ = trace ("Available specialized structs: " ++ show (structNames syms)) ()
  let newFd =
        fd
          { funcParams = map (\(Param n t) -> Param n (updateTypeWithSpecialization t syms)) (funcParams fd),
            funcRetType = updateTypeWithSpecialization (funcRetType fd) syms
          }
  let _ = trace ("Updated params: " ++ show (funcParams newFd)) ()
  newFd

-- | A final pass to update all function definitions in the symbol table.
updateAllFunctionDefs :: SymbolTable -> SymbolTable
updateAllFunctionDefs syms = do
  let _ = trace ("== updateAllFunctionDefs ===") ()
  let _ = trace ("Current funcDefs in sym table: " ++ show (funcDefs syms)) ()
  syms {funcDefs = M.map (\fd -> updateFuncDef fd syms) (funcDefs syms)}
