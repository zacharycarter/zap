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
  , emptySymbolTable
  , getSpecializedName
  , lookupStruct
  , registerStruct
  , registerParamStruct
  , registerSpecializedStruct
  , typeToSuffix
  , registerVarType
  , lookupVarType
  ) where

import qualified Data.Map.Strict as M

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
  = DFunc String [Param] Type Expr
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

data SymbolTable = SymbolTable
  { nextStructId :: StructId
  , structDefs :: M.Map StructId StructDef
  , structNames :: M.Map String StructId
  , varTypes :: M.Map String Type
  } deriving (Show, Eq)

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable
  { nextStructId = StructId 0  -- Start IDs at 0
  , structDefs = M.empty       -- No struct definitions initially
  , structNames = M.empty      -- No struct names initially
  , varTypes = M.empty         -- No var types initially
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

substituteStructParams :: StructDef -> [(String, Type)] -> StructDef
substituteStructParams def substitutions = def
  { structFields = map substituteField (structFields def)
  }
  where
    substituteField (name, fieldType) =
      (name, foldr substitute fieldType substitutions)
    substitute (param, replType) t = substituteTypeParam param replType t

substituteTypeParam :: String -> Type -> Type -> Type
substituteTypeParam param replacement = go
  where
    go t = case t of
      TypeParam name | name == param -> replacement
      TypeArray inner -> TypeArray (go inner)
      TypeStruct sid name -> TypeStruct sid name  -- Keep structs as-is
      _ -> t  -- Other types stay unchanged

registerSpecializedStruct :: String -> StructDef -> Type -> SymbolTable -> (StructId, SymbolTable)
registerSpecializedStruct specializationName baseDef paramType st =
    let substitutions = zip (structParams baseDef) [paramType]
        specializedDef = (substituteStructParams baseDef substitutions)
          { structName = specializationName  -- Use the name passed in directly
          , structParams = []  -- No type params in specialized version
          }
        sid = nextStructId st
        st' = st
          { nextStructId = StructId (case sid of StructId n -> n + 1)
          , structDefs = M.insert sid specializedDef (structDefs st)
          , structNames = M.insert specializationName sid (structNames st)
          }
    in (sid, st')

getSpecializedName :: String -> Type -> String
getSpecializedName base paramType = base ++ "_" ++ typeToSuffix paramType

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
