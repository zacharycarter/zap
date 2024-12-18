{-# LANGUAGE OverloadedStrings #-}
module Zap.AST
  ( Program(..)
  , TopLevel(..)
  , Decl(..)
  , Param(..)
  , Type(..)
  , Op(..)
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
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Lt
  | Gt
  | And
  | Or
  | Dot
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
  deriving (Show, Eq)

data Program = Program [TopLevel]
  deriving (Show, Eq)

data BlockScope = BlockScope
  { blockLabel :: String
  , blockExprs :: [Expr]
  , blockResult :: Maybe Expr
  } deriving (Show, Eq)

data Expr
  = NumLit NumType String      -- Store number as string to preserve precision
  | VecLit VecType [Expr]      -- Vector literal with components
  | StrLit String
  | BoolLit Bool
  | Var String
  | Let String Expr
  | Print Expr
  | Block BlockScope
  | Break String
  | Result Expr
  | BinOp Op Expr Expr
  | If Expr Expr Expr
  | Call String [Expr]
  | StructLit String [(String, Expr)]  -- Struct construction
  | FieldAccess Expr String            -- Access struct field
  | ArrayLit Type [Expr]               -- Array literal
  | Index Expr Expr                    -- Array indexing
  deriving (Show, Eq)
