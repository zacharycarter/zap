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
    isFunctionLabel,
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
    structParams :: [Type],
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
  = DFunc String [Type] [Param] Type Expr
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
    funcTypeParams :: [Type],
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

lookupStruct :: StructId -> SymbolTable -> Maybe StructDef
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

registerParamStruct :: String -> [Type] -> [(String, Type)] -> SymbolTable -> (StructId, SymbolTable)
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

substituteStructParams :: StructDef -> [(Type, Type)] -> SymbolTable -> (StructDef, SymbolTable)
substituteStructParams def substitutions st = do
  trace
    ( "\n=== substituteStructParams ===\n"
        ++ "Input def: "
        ++ show def
        ++ "Using nextStructId: "
        ++ show (nextStructId st)
        ++ "\n"
        ++ "For struct: "
        ++ structName def
        ++ "\nSubstitutions: "
        ++ show substitutions
        ++ "\nPre-substitution structDefs: "
        ++ show (M.toList $ structDefs st)
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

        newDef =
          StructDef
            { structName = structName def, -- KEEP original name during substitution
              structParams = [], -- Clear params
              structFields = newFields,
              structId = structId def
            }
     in trace ("Post-substitution def: " ++ show newDef) $
          (newDef, finalST)

----------------------------------------------------------------------
-- Single field substitution => returns (Type, SymbolTable)
----------------------------------------------------------------------
substituteFieldType ::
  S.Set StructId -> -- 'seen' set
  String -> -- field name (for debug)
  Type -> -- original field type
  [(Type, Type)] -> -- top-level (param -> replacement) list
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
                      -- Recursively apply substitution to resolved type
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
          -- Not a struct => fallback to param substitution
          ----------------------------------------------------------------
          p@(TypeParam _) ->
            -- First try to substitute the type parameter
            trace ("  Substituting parameter" ++ show p) $
              case lookup p substitutions of
                Just replacement ->
                  trace
                    ("    Replacement: " ++ show replacement)
                    (replacement, st)
                Nothing -> (t, st)
          _ ->
            let t' = foldr (substituteOneTypeParam) t substitutions
             in trace
                  ("  Basic substitution result: " ++ show t')
                  (t', curST)

    -- fallback param-substitution for e.g. TypeParam "T"
    substituteOneTypeParam :: (Type, Type) -> Type -> Type
    substituteOneTypeParam (paramType, replType) ty =
      trace
        ( "\n=== substitute' ===\n"
            ++ "Param: "
            ++ show paramType
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
            | TypeParam p == paramType ->
                trace
                  ( "Substituting param "
                      ++ show p
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
  [(Type, Type)] ->
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
                                trace
                                  ("  Found existing specialized struct with name:" ++ show specializedName ++ " and id: " ++ show specSid)
                                  (TypeStruct specSid specializedName, st)
                              Nothing ->
                                -- Update any parent structs that use this type
                                let updatedSt = updateParentStructs st name specializedName sid
                                 in trace ("      Updated parent structs in symbol table: " ++ show updatedSt) $
                                      let (sid', newSt) =
                                            registerSpecializedStruct specializedName def (map snd bridging) updatedSt
                                       in trace
                                            ("    Substitution for nested type not found. Registering...")
                                            (TypeStruct sid' specializedName, newSt)
                    else (t, st)
          else (t, st)
      Nothing -> (t, st)
substituteNestedType st _ t = (t, st)

-- New helper function to update parent structs
updateParentStructs :: SymbolTable -> String -> String -> StructId -> SymbolTable
updateParentStructs st baseName specializedName specializedSid =
  let updatedStructs =
        M.mapWithKey
          ( \sid def ->
              if any (\(_, fieldType) -> fieldType `usesType` baseName) (structFields def)
                then -- Update the struct name to the specialized name

                  def
                    { structName = getSpecializedStructName (structName def) sid st,
                      structFields =
                        updateFieldTypes (structFields def) baseName specializedName specializedSid,
                      structParams = [] -- Clear the type parameters for the specialized struct
                    }
                else def
          )
          (structDefs st)
      -- Update structNames with the new specialized name and ID
      -- We need to remove the old name and add the new one
      updatedNames =
        foldl
          ( \nameMap (sid, def) ->
              if any (\(_, fieldType) -> fieldType `usesType` baseName) (structFields def)
                then
                  let oldName = structName def
                      newName = getSpecializedStructName oldName sid st
                   in (M.delete oldName nameMap) `M.union` (M.singleton newName sid)
                else nameMap
          )
          (structNames st)
          (M.toList updatedStructs)
   in trace ("\n=== updateParentStructs ===" ++ "updatedStructs: " ++ show updatedStructs ++ " updatedNames: " ++ show updatedNames) $
        st {structDefs = updatedStructs, structNames = updatedNames}

-- Helper to check if a type uses another type in its definition
usesType :: Type -> String -> Bool
usesType (TypeStruct _ n) baseName = n == baseName
usesType (TypeUnresolved n) baseName = n == baseName
usesType _ _ = False

-- Helper to update field types within a struct definition
updateFieldTypes :: [(String, Type)] -> String -> String -> StructId -> [(String, Type)]
updateFieldTypes fields baseName specializedName specializedSid =
  map
    ( \(name, fieldType) ->
        ( name,
          case fieldType of
            TypeUnresolved n | n == baseName -> TypeStruct specializedSid specializedName
            TypeStruct _ n | n == baseName -> TypeStruct specializedSid specializedName
            _ -> fieldType
        )
    )
    fields

-- | Perform a *multi-parameter* substitution all at once, so that if
--   def has structParams = ["S","T"] and we pass [("S",Int64),("T",Int32)],
--   we can produce "Pair_i64_i32" in one pass.
substituteAllParamsWithSymbols ::
  -- | The entire (param -> replacement Type) list, e.g. [("S",TypeNum Int64), ("T",TypeNum Int32)]
  [(Type, Type)] ->
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
  Type ->
  -- | single param replacement
  Type ->
  -- | the entire sub list
  [(Type, Type)] ->
  -- | the type we are substituting into
  Type ->
  SymbolTable ->
  Type
substituteOneParam paramType repl fullList t st =
  trace
    ( "\n=== substituteOneParam (with extra logging) ===\n"
        ++ "param: "
        ++ show paramType
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
        | t == paramType ->
            trace
              ( "[DEBUG] We found a matching param ("
                  ++ show paramType
                  ++ "), substituting with "
                  ++ show repl
              )
              repl
      -- If it is a struct type that *may* have the parameter(s):
      TypeStruct sid structName' ->
        let debug1 = "[DEBUG] TypeStruct: " ++ structName' ++ " (sid=" ++ show sid ++ ")"
         in trace debug1 $
              case lookupStruct sid st of
                Just def ->
                  let childParams = structParams def
                      debug2 =
                        "[DEBUG] "
                          ++ structName'
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
                        getMultiParamName structName' (map snd relevantSubs)
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
                                    ++ show paramType
                                    ++ " is not relevant to child struct: "
                                    ++ structName'
                                    ++ " => returning original t: "
                                    ++ show t
                                )
                                t
                Nothing ->
                  trace
                    ( "[DEBUG] Could NOT look up struct sid="
                        ++ show sid
                        ++ " named "
                        ++ structName'
                        ++ " => returning t"
                    )
                    t
      -- Recur if array, etc. (unchanged):
      TypeArray inner ->
        let newInner = substituteOneParam paramType repl fullList inner st
         in newInner
      -- Otherwise, do nothing:
      other ->
        trace
          ( "[DEBUG] Not a matching TypeParam/TypeStruct, returning as is => "
              ++ show other
          )
          other

substituteTypeParamWithSymbols :: Type -> Type -> Type -> SymbolTable -> Type
substituteTypeParamWithSymbols paramType replacement t st =
  trace
    ( "\n=== substituteTypeParamWithSymbols ===\n"
        ++ "Param: "
        ++ show paramType
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
        | TypeParam name == paramType ->
            trace
              ("Substituting param " ++ name)
              replacement
      -- If it is a struct type that *does* have the parameter(s):
      TypeStruct sid structName' ->
        trace ("Found struct, looking up struct with name: " ++ show structName' ++ " and sid: " ++ show sid) $
          case lookupStruct sid st of
            Just def ->
              -- Check if param is in def's structParams
              if paramType `elem` structParams def
                then -- Gather *all* relevant type-substitutions that match def's structParams

                  let childParams = structParams def
                      relevantSubs =
                        filter
                          (\(outerType, _ty) -> outerType `elem` childParams)
                          [(paramType, replacement)]
                      -- \^ we add the single (param->replacement) plus
                      --   any others in your environment if needed…

                      -- Build final specialized name once
                      specializedName =
                        getMultiParamName
                          structName'
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
                    ++ show structName'
                    ++ " and sid: "
                    ++ show sid
                )
                t

      -- Recur if array, etc.
      TypeArray inner -> do
        trace ("Found type array: " ++ show inner ++ " recurring...") $
          TypeArray (substituteTypeParamWithSymbols paramType replacement inner st)
      _ -> t
  where
    -- If you have a larger "substitutions" map in scope, gather them here:
    -- For example, you might pass in all `(funcTypeParam, Type)` pairs too:
    allOtherParams = [] -- stub

substituteTypeParam :: Type -> Type -> Type -> Type
substituteTypeParam paramType replacement = go
  where
    go t = case t of
      p@(TypeParam _) | p == paramType -> replacement
      TypeArray inner -> TypeArray (go inner)
      TypeStruct sid name ->
        -- If this struct refers to Box[T], substitute T recursively
        case lookupStruct sid emptySymbolTable of
          Just def
            | paramType `elem` structParams def ->
                -- This is a parameterized struct, substitute the type parameter
                TypeStruct sid (name ++ "_" ++ typeToSuffix replacement)
          _ -> t
      _ -> t

----------------------------------------------------------------------
-- The function that tries to do a record update
-- => We must separate the record updates from the (StructDef, SymbolTable) call
----------------------------------------------------------------------
registerSpecializedStruct :: String -> StructDef -> [Type] -> SymbolTable -> (StructId, SymbolTable)
registerSpecializedStruct specializationName baseDef paramTypes st = do
  let sid = nextStructId st
  trace
    ( "\n=== registerSpecializedStruct ===\n"
        ++ "Base struct: "
        ++ show baseDef
        ++ "\n"
        ++ "Specialization Name: "
        ++ specializationName
        ++ "\n"
        ++ "Param types: "
        ++ show paramTypes
        ++ "\n"
        ++ "Current symbol table: "
        ++ show st
        ++ "\n"
        ++ "Will use nextStructId: "
        ++ show sid
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
              trace ("Found definition: " ++ show existingDef) $
                (existingSid, st)
            Nothing -> do
              let substitutions = zip (structParams baseDef) paramTypes
                  (specializedDef, st1) = substituteStructParams baseDef substitutions st
                  newDef =
                    specializedDef
                      { structName = specializationName,
                        structId = sid,
                        structParams = []
                      }
                  st2 =
                    st1
                      { nextStructId = incSid sid,
                        structDefs = M.insert sid newDef (structDefs st1),
                        structNames = M.insert specializationName sid (structNames st1)
                      }
              trace
                ( "\n=== Creating new specialized struct ===\n"
                    ++ "New SID: "
                    ++ show sid
                    ++ "\n"
                    ++ "New def: "
                    ++ show newDef
                    ++ "\n"
                    ++ "Updated nextStructId: "
                    ++ show (nextStructId st2)
                )
                $ registerConstructor specializationName sid newDef st2
      Nothing -> do
        let substitutions = zip (structParams baseDef) paramTypes
            (specializedDef, st1) = substituteStructParams baseDef substitutions st
            newDef =
              specializedDef
                { structName = specializationName,
                  structId = sid,
                  structParams = []
                }
            st2 =
              st1
                { nextStructId = incSid sid,
                  structDefs = M.insert sid newDef (structDefs st1),
                  structNames = M.insert specializationName sid (structNames st1)
                }
        trace
          ( "\n=== Creating fresh specialized struct ===\n"
              ++ "Using SID: "
              ++ show sid
              ++ "\n"
              ++ "Struct def: "
              ++ show newDef
              ++ "\n"
              ++ "Current structDefs: "
              ++ show (structDefs st2)
              ++ "\n"
              ++ "Current structNames: "
              ++ show (structNames st2)
          )
          $ registerConstructor specializationName sid newDef st2

registerConstructor :: String -> StructId -> StructDef -> SymbolTable -> (StructId, SymbolTable)
registerConstructor name sid def st =
  trace
    ( "\n=== registerConstructor ===\n"
        ++ "Registering constructor for: "
        ++ name
        ++ "\n"
        ++ "Using SID: "
        ++ show sid
        ++ "\n"
        ++ "Def: "
        ++ show def
        ++ "\n"
        ++ "Current nextStructId: "
        ++ show (nextStructId st)
    )
    $ let constructorDef =
            FunctionDef
              { funcName = name,
                funcParams = [Param fname ftype | (fname, ftype) <- structFields def],
                funcTypeParams = [],
                funcRetType = TypeStruct sid name,
                funcBody =
                  Block name [] $
                    Just $
                      StructLit name [(f, Var f) | (f, _) <- structFields def]
              }
          st' = st {funcDefs = M.insert name constructorDef (funcDefs st)}
       in trace
            ( "Constructor registered. Final state:\n"
                ++ "  funcDefs: "
                ++ show (funcDefs st')
                ++ "\n"
                ++ "  nextStructId: "
                ++ show (nextStructId st')
            )
            $ (sid, st')

incSid :: StructId -> StructId
incSid (StructId n) = StructId (n + 1)

findNameForId :: StructId -> M.Map String StructId -> Maybe String
findNameForId sid nameMap =
  let matches = M.toList $ M.filter (== sid) nameMap
   in case matches of
        (name, _) : _ -> Just name
        [] -> Nothing

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
    substituteAllInParam :: [(Type, Type)] -> SymbolTable -> Param -> Param
    substituteAllInParam fullList st' (Param nm ty) =
      let newTy = substituteAllParamsWithSymbols fullList ty st'
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
  trace "\n=== verifySymbolTable ===" $
    trace ("Checking structNames: " ++ show (M.toList $ structNames st)) $
      trace ("Against structDefs: " ++ show (M.toList $ structDefs st)) $
        do
          -- Check all names have valid defs
          forM_ (M.toList $ structNames st) $ \(name, sid) -> do
            trace ("\nChecking name " ++ name ++ " (sid: " ++ show sid ++ ")") $
              case M.lookup sid (structDefs st) of
                Nothing -> Left $ "Found name " ++ name ++ " with sid " ++ show sid ++ " but no matching def"
                Just def -> do
                  trace ("Found def: " ++ show def) $
                    let baseMatch = structName def == name
                        specMatch = case break (== '_') name of
                          (base, '_' : _) -> structName def == base
                          _ -> False
                     in do
                          trace ("baseMatch: " ++ show baseMatch ++ ", specMatch: " ++ show specMatch) $
                            if not (baseMatch || specMatch)
                              then Left $ "Name mismatch: " ++ name ++ " maps to def named " ++ structName def
                              else Right ()

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
