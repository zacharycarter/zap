{-# LANGUAGE OverloadedStrings #-}
module Zap.IR.Types
  ( IRType(..)
  , IRNumType(..)
  , IRVecType(..)
  , getTypeSize
  , getTypeAlign
  , isSimpleType
  ) where

import Data.Functor.Identity
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

data IRNumType
  = IRInt32
  | IRInt64
  | IRFloat32
  | IRFloat64
  deriving (Eq, Show, Ord)

data IRVecType
  = IRVec2 IRNumType
  | IRVec3 IRNumType
  | IRVec4 IRNumType
  deriving (Eq, Show, Ord)

data IRType
  = IRTypeNum IRNumType
  | IRTypeVec IRVecType
  | IRTypeString
  | IRTypeBool
  | IRTypeStruct T.Text [(T.Text, IRType)]
  | IRTypeArray IRType
  deriving (Eq, Show)

getTypeSize :: Monad m => IRType -> m Integer
getTypeSize typ = return $ case typ of
  IRTypeNum nt -> case nt of
    IRInt32 -> 4
    IRInt64 -> 8
    IRFloat32 -> 4
    IRFloat64 -> 8
  IRTypeVec vt -> case vt of
    IRVec2 _ -> 8
    IRVec3 _ -> 12
    IRVec4 _ -> 16
  IRTypeString -> 8
  IRTypeBool -> 1
  IRTypeStruct _ fields -> sum $ map (runIdentity . getTypeSize . snd) fields
  IRTypeArray _ -> 8

getTypeAlign :: IRType -> Integer
getTypeAlign (IRTypeNum nt) = case nt of
  IRInt32 -> 4
  IRInt64 -> 8
  IRFloat32 -> 4
  IRFloat64 -> 8
getTypeAlign (IRTypeVec vt) = case vt of
  IRVec2 _ -> 8
  IRVec3 _ -> 16
  IRVec4 _ -> 16
getTypeAlign IRTypeString = 8
getTypeAlign IRTypeBool = 1
getTypeAlign (IRTypeStruct _ fields) = maximum $ map (getTypeAlign . snd) fields
getTypeAlign (IRTypeArray _) = 8

isSimpleType :: IRType -> Bool
isSimpleType (IRTypeNum _) = True
isSimpleType IRTypeBool = True
isSimpleType _ = False
