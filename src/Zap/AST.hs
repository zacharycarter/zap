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
  , BlockScope(..)
  , NumType(..)
  , VecType(..)
  ) where

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

data Type
  = TypeNum NumType
  | TypeVec VecType
  | TypeString
  | TypeBool
  | TypeStruct String [(String, Type)]  -- Name and fields
  | TypeArray Type                      -- Array of any type
  | TypeVoid
  | TypeAny
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | EqEq
  | Lt
  | Gt
  | And
  | Or
  | Dot
  | AddAssign
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

data BlockScope = BlockScope
  { blockLabel :: String
  , blockExprs :: [Expr]
  , blockResult :: Maybe Expr
  } deriving (Show, Eq)

data Literal
  = IntLit String (Maybe NumType)
  | FloatLit String (Maybe NumType)
  | StringLit String
  | BooleanLit Bool
  deriving (Show, Eq)

data Expr
  = NumLit NumType String      -- Store number as string to preserve precision
  | VecLit VecType [Expr]      -- Vector literal with components
  | StrLit String
  | BoolLit Bool
  | Var String
  | Let String Expr
  | Block BlockScope
  | Break (Maybe String)
  | Result Expr
  | BinOp Op Expr Expr
  | If Expr Expr Expr
  | Call String [Expr]
  | StructLit String [(String, Expr)]  -- Struct construction
  | FieldAccess Expr String            -- Access struct field
  | ArrayLit Type [Expr]               -- Array literal
  | Index Expr Expr                    -- Array indexing
  | VarLit String Expr
  | While Expr Expr  -- Condition and body expressions
  | VarDecl String Expr  -- Variable declaration with initial value
  | Assign String Expr -- Assignment operator
  | AssignOp String Op Expr  -- Assignment with operator (e.g. +=)
  | Lit Literal
  deriving (Show, Eq)
