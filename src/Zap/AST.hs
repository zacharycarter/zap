{-# LANGUAGE OverloadedStrings #-}

module Zap.AST
  ( Program(..)
  , TopLevel(..)
  , Decl(..)
  , Param(..)
  , Type(..)
  , Op(..)
  , Literal(..)
  , Expr(..)
  , NumType(..)
  , VecType(..)
  , SymbolTable(..)
  , StructId(..)
  , StructDef(..)
  , FunctionDef(..)
  , emptySymbolTable
  , getSpecializedName
  , getSpecializedFuncName
  , lookupStruct
  , registerStruct
  , registerParamStruct
  , registerSpecializedStruct
  , typeToSuffix
  , registerVarType
  , lookupVarType
  , specializeFunctionDef
  , substituteTypeParam
  , substituteTypeParamWithSymbols
  ) where

import qualified Data.Map.Strict as M
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
  { structName :: String
  , structParams :: [String]
  , structFields :: [(String, Type)]
  , structId :: StructId
  } deriving (Show, Eq)

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
  deriving (Show, Eq)

data Op
  = Add | Sub | Mul | Div | Mod           -- Arithmetic
  | Lt | LtEq | Gt | GtEq                 -- Comparison
  | Eq | NotEq                            -- Equality
  | And | Or | Not                        -- Logical
  | Dot                                   -- Field access
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
  | Break (Maybe String)
  | Result Expr
  | BinOp Op Expr Expr
  | If Expr Expr Expr
  | Call String [Expr]
  | StructLit String [(String, Expr)]  -- Struct construction
  | FieldAccess Expr String            -- Access struct field
  | ArrayLit Type [Expr]               -- Array literal
  | Index Expr Expr                    -- Array indexing
  | While Expr Expr  -- Condition and body expressions
  | VarDecl String Expr  -- Variable declaration with initial value
  | Assign String Expr -- Assignment operator
  | AssignOp String Op Expr  -- Assignment with operator (e.g. +=)
  | Lit Literal
  deriving (Show, Eq)

data VarType = VarType
  { varName :: String
  , varType :: Type
  } deriving (Show, Eq)

data FunctionDef = FunctionDef
  { funcName :: String
  , funcParams :: [Param]
  , funcTypeParams :: [String]
  , funcRetType :: Type
  , funcBody :: Expr
  } deriving (Show, Eq)

data SymbolTable = SymbolTable
  { nextStructId :: StructId
  , structDefs :: M.Map StructId StructDef
  , structNames :: M.Map String StructId
  , varTypes :: M.Map String Type
  , funcDefs :: M.Map String FunctionDef
  } deriving (Show, Eq)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable
  { nextStructId = StructId 0
  , structDefs = M.empty
  , structNames = M.empty
  , varTypes = M.empty
  , funcDefs = M.empty
  }

lookupStruct :: StructId -> SymbolTable -> Maybe StructDef
lookupStruct sid st = M.lookup sid (structDefs st)

registerStruct :: String -> [(String, Type)] -> SymbolTable -> (StructId, SymbolTable)
registerStruct name fields st =
    let sid = nextStructId st
        def = StructDef
          { structName = name
          , structParams = []  -- No type parameters
          , structFields = fields
          , structId = sid
          }
        st' = st
          { nextStructId = StructId (case sid of StructId n -> n + 1)
          , structDefs = M.insert sid def (structDefs st)
          , structNames = M.insert name sid (structNames st)
          }
    in (sid, st')

registerParamStruct :: String -> [String] -> [(String, Type)] -> SymbolTable -> (StructId, SymbolTable)
registerParamStruct name params fields st =
    let sid = nextStructId st
        def = StructDef
          { structName = name
          , structParams = params
          , structFields = fields
          , structId = sid
          }
        st' = st
          { nextStructId = StructId (case sid of StructId n -> n + 1)
          , structDefs = M.insert sid def (structDefs st)
          , structNames = M.insert name sid (structNames st)
          }
    in (sid, st')

substituteStructParams :: StructDef -> [(String, Type)] -> SymbolTable -> StructDef
substituteStructParams def substitutions st =
    trace ("\n=== substituteStructParams ===\n" ++
           "Input def: " ++ show def ++ "\n" ++
           "Substitutions: " ++ show substitutions ++ "\n" ++
           "Symbol table: " ++ show st) $
    let result = def { structFields = map substituteField (structFields def) }
    in trace ("Result: " ++ show result) result
  where
    substituteField (name, fieldType) =
        let newType = foldr (substitute st) fieldType substitutions
        in (name, newType)

    substitute :: SymbolTable -> (String, Type) -> Type -> Type
    substitute st (param, replType) t = substituteTypeParamWithSymbols param replType t st

substituteTypeParamWithSymbols :: String -> Type -> Type -> SymbolTable -> Type
substituteTypeParamWithSymbols param replacement t st = go t
  where
    go t = case t of
      TypeParam name | name == param -> replacement
      TypeArray inner -> TypeArray (go inner)
      TypeStruct sid name ->
        -- Look up struct definition to check params
        case lookupStruct sid st of
          Just def | any (`elem` structParams def) [param] ->
            TypeStruct sid (getSpecializedName name replacement)
          _ -> t
      _ -> t

substituteTypeParam :: String -> Type -> Type -> Type
substituteTypeParam param replacement = go
  where
    go t = case t of
      TypeParam name | name == param -> replacement
      TypeArray inner -> TypeArray (go inner)
      TypeStruct sid name ->
        -- If this struct refers to Box[T], substitute T recursively
        case lookupStruct sid emptySymbolTable of
          Just def | param `elem` structParams def ->
            -- This is a parameterized struct, substitute the type parameter
            TypeStruct sid (name ++ "_" ++ typeToSuffix replacement)
          _ -> t
      _ -> t

registerSpecializedStruct :: String -> StructDef -> [Type] -> SymbolTable -> (StructId, SymbolTable)
registerSpecializedStruct specializationName baseDef paramTypes st =
    trace ("\n=== registerSpecializedStruct ===" ++
           "\nBase struct: " ++ show baseDef ++
           "\nParam types: " ++ show paramTypes ++
           "\nCurrent symbol table: " ++ show st) $
    let substitutions = zip (structParams baseDef) paramTypes
        specializedDef = (substituteStructParams baseDef substitutions st)
          { structName = specializationName
          , structParams = []  -- No type params in specialized version
          }
        sid = nextStructId st
        st' = st
          { nextStructId = StructId (case sid of StructId n -> n + 1)
          , structDefs = M.insert sid specializedDef (structDefs st)
          , structNames = M.insert specializationName sid (structNames st)
          }
    in trace ("Updated symbol table: " ++ show st') $
       (sid, st')

-- Helper to instantiate a function definition with concrete types
specializeFunctionDef :: FunctionDef -> [Type] -> SymbolTable -> Either String FunctionDef
specializeFunctionDef def typeArgs _
    | length (funcTypeParams def) /= length typeArgs =
        Left $ "Wrong number of type arguments for " ++ funcName def
    | otherwise = do
        let subst = zip (funcTypeParams def) typeArgs
        let newParams = map (substituteParamType subst) (funcParams def)
        let newRetType = foldr (\(param, typ) -> substituteTypeParam param typ) (funcRetType def) subst
        Right $ def
            { funcName = getSpecializedFuncName (funcName def) typeArgs
            , funcParams = newParams
            , funcTypeParams = []  -- No type params in specialized version
            , funcRetType = newRetType
            }
  where
    substituteParamType subst (Param name typ) =
        Param name $ foldr (\(param, repl) -> substituteTypeParam param repl) typ subst

getSpecializedName :: String -> Type -> String
getSpecializedName base paramType = base ++ "_" ++ typeToSuffix paramType

getSpecializedFuncName :: String -> [Type] -> String
getSpecializedFuncName base typeArgs =
    base ++ "_" ++ T.unpack (T.intercalate "_" (map (T.pack . typeToSuffix) typeArgs))

-- Helper for variable type lookup
lookupVarType :: String -> SymbolTable -> Maybe Type
lookupVarType name st = M.lookup name (varTypes st)

-- Helper to register variable type
registerVarType :: String -> Type -> SymbolTable -> SymbolTable
registerVarType name typ st = st { varTypes = M.insert name typ (varTypes st) }

typeToSuffix :: Type -> String
typeToSuffix (TypeNum Int32) = "i32"
typeToSuffix (TypeNum Int64) = "i64"
typeToSuffix (TypeNum Float32) = "f32"
typeToSuffix (TypeNum Float64) = "f64"
typeToSuffix _ = error "Unsupported type for specialization"
