{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zap.AST
  ( Program (..),
    TopLevel (..),
    Decl (..),
    Param (..),
    Type (..),
    Op (..),
    Literal (..),
    Expr (..),
    NumType (..),
    VecType (..),
    SpecializedSymbol (..),
    SymbolTable (..),
    StructId (..),
    StructDef (..),
    FunctionDef (..),
    emptySymbolTable,
    getSpecializedName,
    getSpecializedFuncName,
    getSpecializedStructName,
    lookupStruct,
    registerStruct,
    registerParamStruct,
    registerSpecializedStruct,
    typeToSuffix,
    registerVarType,
    lookupVarType,
    specializeFunctionDef,
    substituteTypeParam,
    substituteTypeParamWithSymbols,
    getMultiParamName,
    isFunctionLabel
  )
where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace

data NumType
  = Int32
  | Int64
  | Float32
  | Float64
  deriving (Show, Eq)

data VecType
  = Vec2 NumType
  | Vec3 NumType
  | Vec4 NumType
  deriving (Show, Eq)

newtype StructId = StructId Int
  deriving (Show, Eq, Ord)

data StructDef = StructDef
  { structName :: String,
    structParams :: [String],
    structFields :: [(String, Type)],
    structId :: StructId
  }
  deriving (Show, Eq)

data Type
  = TypeNum NumType
  | TypeVec VecType
  | TypeString
  | TypeBool
  | TypeStruct StructId String
  | TypeArray Type
  | TypeVoid
  | TypeAny
  | TypeParam String
  | TypeUnresolved String
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Mod -- Arithmetic
  | Lt
  | LtEq
  | Gt
  | GtEq -- Comparison
  | Eq
  | NotEq -- Equality
  | And
  | Or
  | Not -- Logical
  | Dot -- Field access
  deriving (Show, Eq)

data Param = Param String Type
  deriving (Show, Eq)

data Decl
  = DFunc String [String] [Param] Type Expr
  | DStruct String [(String, Type)]
  deriving (Show, Eq)

data TopLevel
  = TLDecl Decl
  | TLExpr Expr
  | TLType String Type
  deriving (Show, Eq)

data Program = Program [TopLevel]
  deriving (Show, Eq)

data Literal
  = IntLit String (Maybe NumType)
  | FloatLit String (Maybe NumType)
  | StringLit String
  | BooleanLit Bool
  deriving (Show, Eq)

data Expr
  = Var String
  | Let String Expr
  | Block String [Expr] (Maybe Expr)
  | Break (Maybe String) (Maybe Expr)
  | Result Expr
  | BinOp Op Expr Expr
  | If Expr Expr Expr
  | Call String [Expr]
  | StructLit String [(String, Expr)] -- Struct construction
  | FieldAccess Expr String -- Access struct field
  | ArrayLit Type [Expr] -- Array literal
  | Index Expr Expr -- Array indexing
  | While Expr Expr -- Condition and body expressions
  | VarDecl String Expr -- Variable declaration with initial value
  | Assign String Expr -- Assignment operator
  | AssignOp String Op Expr -- Assignment with operator (e.g. +=)
  | Lit Literal
  deriving (Show, Eq)

data VarType = VarType
  { varName :: String,
    varType :: Type
  }
  deriving (Show, Eq)

data FunctionDef = FunctionDef
  { funcName :: String,
    funcParams :: [Param],
    funcTypeParams :: [String],
    funcRetType :: Type,
    funcBody :: Expr
  }
  deriving (Show, Eq)

data SpecializedSymbol
  = SpecializedFunc FunctionDef
  | SpecializedStruct StructDef
  deriving (Show, Eq)

data SymbolTable = SymbolTable
  { nextStructId :: StructId,
    structDefs :: M.Map StructId StructDef,
    structNames :: M.Map String StructId,
    varTypes :: M.Map String Type,
    funcDefs :: M.Map String FunctionDef
  }
  deriving (Show, Eq)

emptySymbolTable :: SymbolTable
emptySymbolTable =
  SymbolTable
    { nextStructId = StructId 0,
      structDefs = M.empty,
      structNames = M.empty,
      varTypes = M.empty,
      funcDefs = M.empty
    }

lookupStruct sid st =
  trace
    ( "\n=== lookupStruct ===\n"
        ++ "Looking up sid: "
        ++ show sid
        ++ "\n"
        ++ "Available structs: "
        ++ show (M.toList $ structDefs st)
    )
    $ M.lookup sid (structDefs st)

registerStruct :: String -> [(String, Type)] -> SymbolTable -> (StructId, SymbolTable)
registerStruct name fields st =
  let sid = nextStructId st
      def =
        StructDef
          { structName = name,
            structParams = [], -- No type parameters
            structFields = fields,
            structId = sid
          }
      st' =
        st
          { nextStructId = StructId (case sid of StructId n -> n + 1),
            structDefs = M.insert sid def (structDefs st),
            structNames = M.insert name sid (structNames st)
          }
   in case verifySymbolTable st' of
        Left err ->
          trace ("Symbol table verification failed in registerStruct: " ++ err) $
          error "Symbol table verification failed"
        Right () -> (sid, st')

registerParamStruct :: String -> [String] -> [(String, Type)] -> SymbolTable -> (StructId, SymbolTable)
registerParamStruct name params fields st =
  let sid = nextStructId st
      def =
        StructDef
          { structName = name,
            structParams = params,
            structFields = fields,
            structId = sid
          }
      st' =
        st
          { nextStructId = StructId (case sid of StructId n -> n + 1),
            structDefs = M.insert sid def (structDefs st),
            structNames = M.insert name sid (structNames st)
          }
   in case verifySymbolTable st' of
        Left err ->
          trace ("Symbol table verification failed in registerParamStruct: " ++ err) $
          error "Symbol table verification failed"
        Right () -> (sid, st')

----------------------------------------------------------------------
-- This is your new substituteStructParams returning (StructDef, SymbolTable)
----------------------------------------------------------------------
substituteStructParams ::
  StructDef -> -- The struct whose fields we are substituting
  [(String, Type)] -> -- The (param -> replacement) list
  SymbolTable -> -- Current symbol table
  (StructDef, SymbolTable)
substituteStructParams def substitutions st =
  trace
    ( "\n=== substituteStructParams ===\n"
        ++ "Processing struct "
        ++ structName def
        ++ "\n"
        ++ "with type params: "
        ++ show (structParams def)
        ++ "\n"
        ++ "substitutions: "
        ++ show substitutions
        ++ "\n"
        ++ "available structs: "
        ++ show (M.keys $ structDefs st)
    )
    $
    -- We'll fold over each field, accumulating (newFields, updatedSymTab).
    let (newFields, finalST) =
          foldl
            ( \(accFields, curST) (fname, ftype) ->
                let (newType, curST') =
                      substituteFieldType
                        (S.singleton (structId def))
                        fname
                        ftype
                        substitutions
                        curST
                 in (accFields ++ [(fname, newType)], curST')
            )
            ([], st)
            (structFields def)

        outDef = def {structFields = newFields}
     in trace ("Final substituted fields: " ++ show newFields) (outDef, finalST)

----------------------------------------------------------------------
-- Single field substitution => returns (Type, SymbolTable)
----------------------------------------------------------------------
substituteFieldType ::
  S.Set StructId -> -- 'seen' set
  String -> -- field name (for debug)
  Type -> -- original field type
  [(String, Type)] -> -- top-level (param -> replacement) list
  SymbolTable ->
  (Type, SymbolTable)
substituteFieldType seen fieldName fieldType substitutions st =
  trace
    ( "\n=== substituteFieldType ==="
        ++ "\nField name: "
        ++ fieldName
        ++ "\nOriginal type: "
        ++ show fieldType
        ++ "\nSubstitutions attempting: "
        ++ show substitutions
        ++ "\nSeen structs: "
        ++ show seen
    )
    $ let (newType, st2) = go fieldType st
       in trace ("New field type: " ++ show newType) (newType, st2)
  where
    go :: Type -> SymbolTable -> (Type, SymbolTable)
    go t curST =
      trace
        ( "\n=== substituteFieldType (go) ===\n"
            ++ "Field type: "
            ++ show t
            ++ "\n"
            ++ "Seen structs: "
            ++ show seen
            ++ "\n"
            ++ "Current substitutions: "
            ++ show substitutions
            ++ "\n"
            ++ "Current symbol table structs: "
            ++ show (M.keys $ structDefs curST)
        )
        $ case t of
          ----------------------------------------------------------------
          -- TypeUnresolved name => see if we can fix it via structNames
          ----------------------------------------------------------------
          TypeUnresolved name ->
            trace ("Resolving unresolved type: " ++ name) $
              case M.lookup name (structNames curST) of
                Just sid ->
                  let resolvedType = TypeStruct sid name
                      (specType, st3) =
                        substituteNestedType curST substitutions resolvedType
                   in trace
                        ( "Resolved unresolved type "
                            ++ name
                            ++ " => "
                            ++ show specType
                        )
                        (specType, st3)
                Nothing ->
                  trace
                    ( "No struct named "
                        ++ name
                        ++ " in structNames; leaving as is"
                    )
                    (t, curST)
          ----------------------------------------------------------------
          -- Already a TypeStruct => see if child struct is param'd
          ----------------------------------------------------------------
          TypeStruct sid nm ->
            trace
              ( "Found struct type: "
                  ++ show nm
                  ++ " with sid "
                  ++ show sid
              )
              $ case lookupStruct sid curST of
                Nothing ->
                  trace
                    ("  No struct definition found for sid: " ++ show sid)
                    (t, curST)
                Just childDef ->
                  trace ("  Found struct definition: " ++ show childDef) $
                    if not (null (structParams childDef))
                      then
                        let childParams = structParams childDef
                            -- Instead of matching by childParam == parentParam,
                            -- unify them indexwise, so childParam[i] gets the i-th parent type.
                            -- Only proceed if lengths match:
                            childSubs =
                              if length childParams == length substitutions
                                then zip childParams (map snd substitutions)
                                else []
                         in trace
                              ( "  Attempting bridging for child params "
                                  ++ show childParams
                                  ++ " => "
                                  ++ show childSubs
                              )
                              $ if not (null childSubs)
                                then
                                  let specializedName =
                                        nm ++ "_" ++ concatMap (typeToSuffix . snd) childSubs
                                   in trace ("  Looking up specialized struct with name: " ++ show specializedName) $
                                        case M.lookup specializedName (structNames curST) of
                                          Just specSid ->
                                            trace
                                              ("  Found existing specialized struct with name:" ++ show specializedName ++ " and id: " ++ show specSid)
                                              (TypeStruct specSid specializedName, curST)
                                          Nothing ->
                                            let (sid', st') =
                                                  registerSpecializedStruct
                                                    specializedName
                                                    childDef
                                                    (map snd childSubs)
                                                    curST
                                             in trace ("No existing specialized struct found. Registering new specialization...") $
                                                (TypeStruct sid' specializedName, st')
                                else
                                  trace
                                    ("  bridging gave no childSubs => leaving " ++ nm)
                                    (t, curST)
                      else
                        if S.member sid seen
                          then
                            trace
                              ( "  Avoiding cycle on concrete struct: "
                                  ++ show sid
                              )
                              (t, curST)
                          else
                            trace
                              ("  Using concrete struct as is => " ++ nm)
                              (t, curST)
          ----------------------------------------------------------------
          -- Not a struct => fallback to param substitution
          ----------------------------------------------------------------
          TypeParam p ->
            -- First try to substitute the type parameter
            trace ("  Substituting parameter" ++ show p) $
              case lookup p substitutions of
                Just replacement ->
                  trace ("    Replacement: " ++ show replacement)
                  (replacement, st)
                Nothing -> (t, st)

          _ ->
            let t' = foldr (substituteOneParam) t substitutions
            in trace
               ("  Basic substitution result: " ++ show t')
               (t', curST)

    -- fallback param-substitution for e.g. TypeParam "T"
    substituteOneParam :: (String, Type) -> Type -> Type
    substituteOneParam (param, replType) ty =
      trace
        ( "\n=== substitute' ===\n"
            ++ "Param: "
            ++ param
            ++ "\n"
            ++ "Replacement: "
            ++ show replType
            ++ "\n"
            ++ "Target type: "
            ++ show ty
            ++ "\n"
            ++ "Known structs: "
            ++ show (M.keys $ structNames st)
        )
        $ case ty of
          TypeParam p
            | p == param ->
                trace
                  ( "Substituting param "
                      ++ p
                      ++ " with "
                      ++ show replType
                  )
                  replType
          _ -> ty

----------------------------------------------------------------------
-- For nested references we also want (Type, SymbolTable).
----------------------------------------------------------------------
substituteNestedType ::
  SymbolTable ->
  [(String, Type)] ->
  Type ->
  (Type, SymbolTable)
substituteNestedType st subs t@(TypeStruct sid name) =
  trace
    ( "\n=== substituteNestedType ===\n"
        ++ "  Processing struct: "
        ++ name
        ++ "\n"
        ++ "    With substitutions: "
        ++ show subs
        ++ "\n"
        ++ "    With symbol table: "
        ++ show st
    )
    $ case lookupStruct sid st of
      Just def ->
        if not (null (structParams def))
          then
            let childParams = structParams def
                -- unify childParams to the parent's substitution types in order:
                bridging =
                  if length childParams == length subs
                    then zip childParams (map snd subs)
                    else []
             in trace ("    Child params: " ++ show childParams ++ "\n") $
                  if not (null bridging)
                    then
                      let specializedName =
                            name ++ "_" ++ concatMap (typeToSuffix . snd) bridging
                       in trace ("    Specialized name: " ++ show specializedName ++ "\n") $
                            case M.lookup specializedName (structNames st) of
                              Just specSid ->
                                trace ("  Found existing specialized struct with name:" ++ show specializedName ++ " and id: " ++ show specSid)
                                (TypeStruct specSid specializedName, st)
                              Nothing ->
                                -- optionally register or skip
                                let (sid', st') =
                                      registerSpecializedStruct
                                        specializedName
                                        def
                                        (map snd bridging)
                                        st
                                 in trace ("    Substitution for nested type not found. Registering...") $
                                      (TypeStruct sid' specializedName, st')
                    else (t, st)
          else (t, st)
      Nothing -> (t, st)
substituteNestedType st _ t = (t, st)

-- | Perform a *multi-parameter* substitution all at once, so that if
--   def has structParams = ["S","T"] and we pass [("S",Int64),("T",Int32)],
--   we can produce "Pair_i64_i32" in one pass.
substituteAllParamsWithSymbols ::
  -- | The entire (param -> replacement Type) list, e.g. [("S",TypeNum Int64), ("T",TypeNum Int32)]
  [(String, Type)] ->
  Type ->
  SymbolTable ->
  Type
substituteAllParamsWithSymbols fullSubList t st =
  trace
    ( "\n=== substituteAllParamsWithSymbols ===\n"
        ++ "fullSubList: "
        ++ show fullSubList
        ++ "\n"
        ++ "Target type: "
        ++ show t
    )
    $ foldr
      ( \(param, replacement) acc ->
          substituteOneParam param replacement fullSubList acc st
      )
      t
      fullSubList

-- A small helper that can look up *all* relevant subs for a struct
substituteOneParam ::
  -- | single param name
  String ->
  -- | single param replacement
  Type ->
  -- | the entire sub list
  [(String, Type)] ->
  -- | the type we are substituting into
  Type ->
  SymbolTable ->
  Type
substituteOneParam param repl fullList t st =
  trace
    ( "\n=== substituteOneParam (with extra logging) ===\n"
        ++ "param: "
        ++ param
        ++ "\nrepl: "
        ++ show repl
        ++ "\nfullList: "
        ++ show fullList
        ++ "\ntarget type: "
        ++ show t
    )
    $ case t of
      -- If this is exactly the type parameter we are substituting:
      TypeParam name
        | name == param ->
            trace
              ( "[DEBUG] We found a matching param ("
                  ++ name
                  ++ "), substituting with "
                  ++ show repl
              )
              repl
      -- If it is a struct type that *may* have the parameter(s):
      TypeStruct sid structName ->
        let debug1 = "[DEBUG] TypeStruct: " ++ structName ++ " (sid=" ++ show sid ++ ")"
         in trace debug1 $
              case lookupStruct sid st of
                Just def ->
                  let childParams = structParams def
                      debug2 =
                        "[DEBUG] "
                          ++ structName
                          ++ " has childParams = "
                          ++ show childParams
                      relevantSubs =
                        filter
                          (\(outerP, _ty) -> outerP `elem` childParams)
                          fullList
                      debug3 =
                        "[DEBUG] relevantSubs for child struct = "
                          ++ show relevantSubs
                      specializedName =
                        getMultiParamName structName (map snd relevantSubs)
                   in trace debug2 $
                        trace debug3 $
                          if not (null relevantSubs)
                            then case M.lookup specializedName (structNames st) of
                              Just specializedSid ->
                                trace
                                  ( "[DEBUG] Found specialized: "
                                      ++ specializedName
                                      ++ " => sid="
                                      ++ show specializedSid
                                  )
                                  $ TypeStruct specializedSid specializedName
                              Nothing ->
                                trace
                                  ( "[DEBUG] No specialized version found for "
                                      ++ specializedName
                                      ++ " returning original t: "
                                      ++ show t
                                  )
                                  t
                            else
                              trace
                                ( "[DEBUG] param "
                                    ++ param
                                    ++ " is not relevant to child struct: "
                                    ++ structName
                                    ++ " => returning original t: "
                                    ++ show t
                                )
                                t
                Nothing ->
                  trace
                    ( "[DEBUG] Could NOT look up struct sid="
                        ++ show sid
                        ++ " named "
                        ++ structName
                        ++ " => returning t"
                    )
                    t
      -- Recur if array, etc. (unchanged):
      TypeArray inner ->
        let newInner = substituteOneParam param repl fullList inner st
         in newInner
      -- Otherwise, do nothing:
      other ->
        trace
          ( "[DEBUG] Not a matching TypeParam/TypeStruct, returning as is => "
              ++ show other
          )
          other

substituteTypeParamWithSymbols :: String -> Type -> Type -> SymbolTable -> Type
substituteTypeParamWithSymbols param replacement t st =
  trace
    ( "\n=== substituteTypeParamWithSymbols ===\n"
        ++ "Param: "
        ++ param
        ++ "\n"
        ++ "Replacement: "
        ++ show replacement
        ++ "\n"
        ++ "Target type: "
        ++ show t
    )
    $ case t of
      -- If this is exactly the type parameter we are substituting:
      TypeParam name
        | name == param ->
            trace
              ("Substituting param " ++ name)
              replacement
      -- If it is a struct type that *does* have the parameter(s):
      TypeStruct sid structName ->
        trace ("Found struct, looking up struct with name: " ++ show structName ++ " and sid: " ++ show sid) $
          case lookupStruct sid st of
            Just def ->
              -- Check if param is in def's structParams
              if param `elem` structParams def
                then -- Gather *all* relevant type-substitutions that match def's structParams

                  let relevantSubs =
                        filter
                          (\(p, _) -> p `elem` structParams def)
                          ([(param, replacement)] ++ allOtherParams)
                      -- \^ we add the single (param->replacement) plus
                      --   any others in your environment if needed…

                      -- Build final specialized name once
                      specializedName =
                        getMultiParamName
                          structName
                          [snd sub | sub <- relevantSubs]
                   in -- Look for or create that specialized struct sid
                      case M.lookup specializedName (structNames st) of
                        Just specializedSid ->
                          trace
                            ( "Found full multi-parameter specialized struct: "
                                ++ specializedName
                            )
                            $ TypeStruct specializedSid specializedName
                        Nothing ->
                          trace
                            ( "No specialized version found for multi-params: "
                                ++ specializedName
                            )
                            t
                else -- If param not relevant to this struct, do nothing
                  trace "Param not relevant to this struct, doing nothing" t
            Nothing -> do
              trace
                ( "Failed looking up struct with name: "
                    ++ show structName
                    ++ " and sid: "
                    ++ show sid
                )
                t

      -- Recur if array, etc.
      TypeArray inner -> do
        trace ("Found type array: " ++ show inner ++ " recurring...") $
          TypeArray (substituteTypeParamWithSymbols param replacement inner st)
      _ -> t
  where
    -- If you have a larger "substitutions" map in scope, gather them here:
    -- For example, you might pass in all `(funcTypeParam, Type)` pairs too:
    allOtherParams = [] -- stub
    getMultiParamName base ts =
      base ++ "_" ++ T.unpack (T.intercalate "_" (map (T.pack . typeToSuffix) ts))

substituteTypeParam :: String -> Type -> Type -> Type
substituteTypeParam param replacement = go
  where
    go t = case t of
      TypeParam name | name == param -> replacement
      TypeArray inner -> TypeArray (go inner)
      TypeStruct sid name ->
        -- If this struct refers to Box[T], substitute T recursively
        case lookupStruct sid emptySymbolTable of
          Just def
            | param `elem` structParams def ->
                -- This is a parameterized struct, substitute the type parameter
                TypeStruct sid (name ++ "_" ++ typeToSuffix replacement)
          _ -> t
      _ -> t

----------------------------------------------------------------------
-- The function that tries to do a record update
-- => We must separate the record updates from the (StructDef, SymbolTable) call
----------------------------------------------------------------------
registerSpecializedStruct ::
  String -> -- specializationName
  StructDef -> -- baseDef
  [Type] -> -- paramTypes
  SymbolTable ->
  (StructId, SymbolTable)
registerSpecializedStruct specializationName baseDef paramTypes st = do
  trace
    ( "\n=== registerSpecializedStruct ===\n"
        ++ "Base struct: "
        ++ show baseDef
        ++ "\nParam types: "
        ++ show paramTypes
        ++ "\nCurrent symbol table: "
        ++ show st
        ++ "\nWill use nextStructId: "
        ++ show (nextStructId st)
    )
    $ case M.lookup specializationName (structNames st) of
      Just existingSid -> do
        trace
          ( "Found existing specialization: "
              ++ specializationName
              ++ " with sid: "
              ++ show existingSid
          )
          $ case M.lookup existingSid (structDefs st) of
            Just existingDef -> do
              trace
                ( "Found definition: "
                    ++ show existingDef
                )
                $ case verifySymbolTable st of
                    Left err -> error $ "Symbol table inconsistent with existing struct: " ++ err
                    Right () -> (existingSid, st)
            Nothing -> do
              -- If we have an ID but no def, we need a new ID
              let sid = nextStructId st
              trace
                ( "No definition found for sid "
                    ++ show existingSid
                    ++ ", creating new definition with sid "
                    ++ show sid
                )
                $ let substitutions = zip (structParams baseDef) paramTypes
                      (tempDef, stTmp) = substituteStructParams baseDef substitutions st
                      -- First clean up the old mapping
                      stCleaned = stTmp {
                        structNames = M.delete specializationName (structNames stTmp)
                      }
                      specializedDef =
                        tempDef
                          { structName = specializationName,
                            structId = sid,
                            structParams = []
                          }
                      updatedSt =
                        stCleaned
                          { nextStructId = incSid sid,
                            structDefs = M.insert sid specializedDef (structDefs stCleaned),
                            structNames = M.insert specializationName sid (structNames stCleaned)
                          }
                      -- Add constructor function
                      constructorDef = FunctionDef
                        { funcName = specializationName
                        , funcParams = [Param fname ftype | (fname, ftype) <- structFields specializedDef]
                        , funcTypeParams = []
                        , funcRetType = TypeStruct sid specializationName
                        , funcBody = Block specializationName [] $ Just $
                            StructLit specializationName [(f, Var f) | (f, _) <- structFields specializedDef]
                        }
                      finalSt = updatedSt { funcDefs = M.insert specializationName constructorDef (funcDefs updatedSt) }
                   in trace
                        ( "Updated symbol table:\n"
                            ++ "- structDefs: "
                            ++ show (structDefs finalSt)
                            ++ "\n- structNames: "
                            ++ show (structNames finalSt)
                            ++ "\n- Added constructor: "
                            ++ show constructorDef
                        )
                        $ case verifySymbolTable finalSt of
                            Left err -> error $ "Symbol table inconsistent after update: " ++ err
                            Right () -> (sid, finalSt)
      Nothing -> do
        let sid = nextStructId st
        trace
          "No existing specialized struct definition found in symbol table. Registering it..."
          $ let substitutions = zip (structParams baseDef) paramTypes
                (tempDef, stTmp) = substituteStructParams baseDef substitutions st
                existingName = findNameForId sid (structNames stTmp)
                stCleaned = case existingName of
                  Just name -> stTmp { structNames = M.delete name (structNames stTmp) }
                  Nothing -> stTmp
                specializedDef =
                  tempDef
                    { structName = specializationName,
                      structId = sid,
                      structParams = []
                    }
                updatedSt =
                  stCleaned
                    { nextStructId = incSid sid,
                      structDefs = M.insert sid specializedDef (structDefs stCleaned),
                      structNames = M.insert specializationName sid (structNames stCleaned)
                    }
                -- Add constructor function for the new specialized type
                constructorDef = FunctionDef
                  { funcName = specializationName
                  , funcParams = [Param fname ftype | (fname, ftype) <- structFields specializedDef]
                  , funcTypeParams = []
                  , funcRetType = TypeStruct sid specializationName
                  , funcBody = Block specializationName [] $ Just $
                      StructLit specializationName [(f, Var f) | (f, _) <- structFields specializedDef]
                  }
                finalSt = updatedSt { funcDefs = M.insert specializationName constructorDef (funcDefs updatedSt) }
             in trace
                  ( "After registration:\n"
                      ++ "- nextStructId: "
                      ++ show (nextStructId finalSt)
                      ++ "\n- structDefs: "
                      ++ show (structDefs finalSt)
                      ++ "\n- structNames: "
                      ++ show (structNames finalSt)
                      ++ "\n- Added constructor: "
                      ++ show constructorDef
                  )
                  $ case verifySymbolTable finalSt of
                      Left err -> error $ "Symbol table inconsistent after new registration: " ++ err
                      Right () -> (sid, finalSt)
  where
    incSid (StructId n) = StructId (n + 1)
    findNameForId :: StructId -> M.Map String StructId -> Maybe String
    findNameForId sid nameMap =
      let matches = M.toList $ M.filter (== sid) nameMap
      in case matches of
        (name, _):_ -> Just name
        [] -> Nothing
    substitutions = zip (structParams baseDef) paramTypes

registerNestedSpecializations :: String -> StructDef -> [Type] -> SymbolTable -> (StructId, SymbolTable)
registerNestedSpecializations specializationName baseDef paramTypes st =
  trace
    ( "\n=== registerNestedSpecializations ===\n"
        ++ "Base struct: "
        ++ show baseDef
        ++ "\nParam types: "
        ++ show paramTypes
    )
    $ let
          -- First register main specialized struct
          (sid, st1) = registerSpecializedStruct specializationName baseDef paramTypes st

          -- For each field, register specialized versions of nested structs
          finalSt = foldr registerAllNested st1 (structFields baseDef)
            where
              registerAllNested (fname, typ) acc = case typ of
                TypeStruct innerSid _ ->
                  case lookupStruct innerSid acc of
                    Just innerDef
                      | not (null (structParams innerDef)) ->
                          let innerName = structName innerDef ++ "_" ++ concatMap typeToSuffix paramTypes
                           in snd $ registerSpecializedStruct innerName innerDef paramTypes acc
                    _ -> acc
                _ -> acc
       in trace("Registered nested specializations. Final symbol table: " ++ show finalSt) $
          (sid, finalSt)

-- Helper to instantiate a function definition with concrete types
specializeFunctionDef ::
  FunctionDef ->
  [Type] ->
  SymbolTable ->
  Either String FunctionDef
specializeFunctionDef def typeArgs st = do
  traceM $ "\n=== specializeFunctionDef ==="
  traceM $ "Base function: " ++ show def
  traceM $ "Type args: " ++ show typeArgs
  traceM $ "Current symbol table: " ++ show st

  when (length (funcTypeParams def) /= length typeArgs) $
    Left $
      "Wrong number of type arguments for " ++ funcName def

  let fullSubList = zip (funcTypeParams def) typeArgs
  traceM $ "Full type substitutions: " ++ show fullSubList

  -- first, fix up parameter types in one pass
  let newParams = map (substituteAllInParam fullSubList st) (funcParams def)
  traceM $ "New params: " ++ show newParams

  -- fix up return type in one pass
  let newRetType = substituteAllParamsWithSymbols fullSubList (funcRetType def) st
  traceM $ "New return type: " ++ show newRetType

  -- rename function
  let oldName = funcName def
  let newName = getSpecializedFuncName oldName typeArgs
  let newBody = renameStructLit (funcBody def) oldName newName

  Right
    def
      { funcName = newName,
        funcParams = newParams,
        funcTypeParams = [], -- fully specialized now
        funcRetType = newRetType,
        funcBody = newBody
      }
  where
    -- For each Param, we do a multi-substitution on its type
    substituteAllInParam :: [(String, Type)] -> SymbolTable -> Param -> Param
    substituteAllInParam fullList st (Param nm ty) =
      let newTy = substituteAllParamsWithSymbols fullList ty st
       in Param nm newTy

    -- rename struct-literal calls from old -> new in the function body
    renameStructLit :: Expr -> String -> String -> Expr
    renameStructLit expr oldNm newNm =
      case expr of
        StructLit litName fields
          | litName == oldNm ->
              StructLit
                newNm
                [ (fieldName, renameStructLit subExpr oldNm newNm)
                  | (fieldName, subExpr) <- fields
                ]
        Block blkName exprs mRes ->
          Block
            blkName
            (map (\e -> renameStructLit e oldNm newNm) exprs)
            (fmap (\r -> renameStructLit r oldNm newNm) mRes)
        Let nm val -> Let nm (renameStructLit val oldNm newNm)
        VarDecl nm val -> VarDecl nm (renameStructLit val oldNm newNm)
        Assign nm val -> Assign nm (renameStructLit val oldNm newNm)
        AssignOp nm op val ->
          AssignOp nm op (renameStructLit val oldNm newNm)
        If c t e ->
          If (go c) (go t) (go e)
        While c b ->
          While (go c) (go b)
        FieldAccess base f ->
          FieldAccess (go base) f
        BinOp op l r ->
          BinOp op (go l) (go r)
        Call name args ->
          -- If the *call* is returning a struct-literal, rename inside args
          Call name (map (\a -> renameStructLit a oldNm newNm) args)
        ArrayLit t arr ->
          ArrayLit (renameType t) (map go arr)
        Index a i -> Index (go a) (go i)
        Break mlbl val -> Break mlbl (fmap go val)
        Result e -> Result (go e)
        _ -> expr -- Var, Lit, etc. unchanged
      where
        go = \ex -> renameStructLit ex oldNm newNm

        renameType t =
          case t of
            TypeStruct sid nm
              | nm == oldNm -> TypeStruct sid newNm
            _ -> t

getSpecializedName :: String -> Type -> String
getSpecializedName base paramType = base ++ "_" ++ typeToSuffix paramType

getSpecializedStructName :: String -> StructId -> SymbolTable -> String
getSpecializedStructName baseName sid st =
  case M.lookup sid (structDefs st) of
    Just def -> structName def
    Nothing -> baseName -- Fallback to base name if not found

getSpecializedFuncName :: String -> [Type] -> String
getSpecializedFuncName base typeArgs =
  base ++ "_" ++ T.unpack (T.intercalate "_" (map (T.pack . typeToSuffix) typeArgs))

-- Helper for variable type lookup
lookupVarType :: String -> SymbolTable -> Maybe Type
lookupVarType name st = M.lookup name (varTypes st)

-- Helper to register variable type
registerVarType :: String -> Type -> SymbolTable -> SymbolTable
registerVarType name typ st = st {varTypes = M.insert name typ (varTypes st)}

typeToSuffix :: Type -> String
typeToSuffix (TypeNum Int32) = "i32"
typeToSuffix (TypeNum Int64) = "i64"
typeToSuffix (TypeNum Float32) = "f32"
typeToSuffix (TypeNum Float64) = "f64"
typeToSuffix (TypeParam param) = param -- Allow params but preserve name
typeToSuffix t = error $ "Unsupported type for specialization: " ++ show t

-- Helper to build specialized name with multiple type parameters
getMultiParamName :: String -> [Type] -> String
getMultiParamName base ts =
  base ++ "_" ++ T.unpack (T.intercalate "_" (map (T.pack . typeToSuffix) ts))

-- | Verify structNames and structDefs are in sync
verifySymbolTable :: SymbolTable -> Either String ()
verifySymbolTable st = do
  -- Check all names have valid defs
  forM_ (M.toList $ structNames st) $ \(name, sid) ->
    case M.lookup sid (structDefs st) of
      Nothing -> Left $ "Found name " ++ name ++ " with sid " ++ show sid ++ " but no matching def"
      Just def
        | structName def /= name ->
            Left $ "Name mismatch: " ++ name ++ " maps to def named " ++ structName def
        | otherwise -> Right ()

  -- Check all defs have valid names
  forM_ (M.toList $ structDefs st) $ \(sid, def) ->
    case M.lookup (structName def) (structNames st) of
      Nothing -> Left $ "Found def for " ++ structName def ++ " but no name mapping"
      Just mappedSid
        | mappedSid /= sid ->
            Left $ "ID mismatch: def has " ++ show sid ++ " but name maps to " ++ show mappedSid
        | otherwise -> Right ()

-- Helper to check if a label is a function label
isFunctionLabel :: String -> Bool
isFunctionLabel label =
  -- A break target is always the function name
  not $ any (`T.isPrefixOf` (T.pack label)) ["if_", "while_", "else_", "end"]
