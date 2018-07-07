{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Pact.Types.Type
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Language types.
--

module Pact.Types.Type
 (
   TypeName(..),
   Arg(..),aInfo,aName,aType,
   FunType(..),ftArgs,ftReturn,
   FunTypes,funTypes,showFunTypes,
   PrimType(..),
   tyInteger,tyDecimal,tyTime,tyBool,tyString,
   tyList,tyObject,tyValue,tyKeySet,tyTable,
   SchemaType(..),
   TypeVarName(..),typeVarName,
   TypeVar(..),tvName,tvConstraint,
   Type(..),tyFunType,tyListType,tySchema,tySchemaType,tyUser,tyVar,
   mkTyVar,mkTyVar',mkSchemaVar,
   isAnyTy,isVarTy,isUnconstrainedTy,canUnifyWith,

   ) where


import Control.Lens
import Data.List
import Control.Monad
import Prelude
import Data.Functor.Classes
import Data.Aeson
import Data.String
import Data.Thyme.Format.Aeson ()
import GHC.Generics (Generic)
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen
import Control.DeepSeq
import Data.Text (Text,unpack)

import Pact.Types.Util
import Pact.Types.Info


newtype TypeName = TypeName Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Pretty,Generic,NFData)
instance Show TypeName where show (TypeName s) = show s

-- | Pair a name and a type (arguments, bindings etc)
data Arg o = Arg {
  _aName :: Text,
  _aType :: Type o,
  _aInfo :: Info
  } deriving (Eq,Ord,Functor,Foldable,Traversable,Generic)

instance NFData o => NFData (Arg o)
instance Show o => Show (Arg o) where show (Arg n t _) = unpack n ++ ":" ++ show t
instance (Pretty o) => Pretty (Arg o)
  where pretty (Arg n t _) = pretty n PP.<> colon PP.<> pretty t

instance Eq1 Arg where
  liftEq eq (Arg a b c) (Arg m n o) = a == m && liftEq eq b n && c == o

-- | Function type
data FunType o = FunType {
  _ftArgs :: [Arg o],
  _ftReturn :: Type o
  } deriving (Eq,Ord,Functor,Foldable,Traversable,Generic)

instance NFData o => NFData (FunType o)
instance Show o => Show (FunType o) where
  show (FunType as t) = "(" ++ unwords (map show as) ++ " -> " ++ show t ++ ")"
instance (Pretty o) => Pretty (FunType o) where
  pretty (FunType as t) = parens (hsep (map pretty as) <+> "->" <+> pretty t)

instance Eq1 FunType where
  liftEq eq (FunType a b) (FunType m n) = liftEq (liftEq eq) a m && liftEq eq b n

-- | use NonEmpty for function types
type FunTypes o = NonEmpty (FunType o)

funTypes :: FunType o -> FunTypes o
funTypes ft = ft :| []
showFunTypes :: Show o => FunTypes o -> String
showFunTypes (t :| []) = show t
showFunTypes ts = show (toList ts)

data PrimType =
  TyInteger |
  TyDecimal |
  TyTime |
  TyBool |
  TyString |
  TyValue |
  TyKeySet
  deriving (Eq,Ord,Generic)

instance NFData PrimType


tyInteger,tyDecimal,tyTime,tyBool,tyString,tyList,tyObject,tyValue,tyKeySet,tyTable :: Text
tyInteger = "integer"
tyDecimal = "decimal"
tyTime = "time"
tyBool = "bool"
tyString = "string"
tyList = "list"
tyObject = "object"
tyValue = "value"
tyKeySet = "keyset"
tyTable = "table"

instance Show PrimType where
  show TyInteger = unpack tyInteger
  show TyDecimal = unpack tyDecimal
  show TyTime = unpack tyTime
  show TyBool = unpack tyBool
  show TyString = unpack tyString
  show TyValue = unpack tyValue
  show TyKeySet = unpack tyKeySet
instance Pretty PrimType where pretty = text . show

data SchemaType =
  TyTable |
  TyObject |
  TyBinding
  deriving (Eq,Ord,Generic)

instance NFData SchemaType
instance Show SchemaType where
  show TyTable = unpack tyTable
  show TyObject = unpack tyObject
  show TyBinding = "binding"
instance Pretty SchemaType where pretty = text . show

newtype TypeVarName = TypeVarName { _typeVarName :: Text }
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Hashable,Pretty,Generic,NFData)
instance Show TypeVarName where show = unpack . _typeVarName

-- | Type variables are namespaced for value types and schema types.
data TypeVar v =
  TypeVar { _tvName :: TypeVarName, _tvConstraint :: [Type v] } |
  SchemaVar { _tvName :: TypeVarName }
  deriving (Functor,Foldable,Traversable,Generic)

instance NFData v => NFData (TypeVar v)
instance Eq (TypeVar v) where
  (TypeVar a _) == (TypeVar b _) = a == b
  (SchemaVar a) == (SchemaVar b) = a == b
  _ == _ = False
instance Ord (TypeVar v) where
  x `compare` y = case (x,y) of
    (TypeVar {},SchemaVar {}) -> LT
    (SchemaVar {},TypeVar {}) -> GT
    (TypeVar a _,TypeVar b _) -> a `compare` b
    (SchemaVar a,SchemaVar b) -> a `compare` b
instance Show v => Show (TypeVar v) where
  show (TypeVar n []) = "<" ++ show n ++ ">"
  show (TypeVar n cs) = "<" ++ show n ++ show cs ++ ">"
  show (SchemaVar n) = "<{" ++ show n ++ "}>"
instance (Pretty v) => Pretty (TypeVar v) where
  pretty (TypeVar n []) = angles (pretty n)
  pretty (TypeVar n cs) = angles (pretty n <+> brackets (hsep (map pretty cs)))
  pretty (SchemaVar n) = angles (braces (pretty n))

instance Eq1 TypeVar where
  liftEq eq (TypeVar a b) (TypeVar m n) = a == m && liftEq (liftEq eq) b n
  liftEq _ (SchemaVar a) (SchemaVar m) = a == m
  liftEq _ _ _ = False


-- | Pact types.
data Type v =
  TyAny |
  TyVar { _tyVar :: TypeVar v } |
  TyPrim PrimType |
  TyList { _tyListType :: Type v } |
  TySchema { _tySchema :: SchemaType, _tySchemaType :: Type v } |
  TyFun { _tyFunType :: FunType v } |
  TyUser { _tyUser :: v }
    deriving (Eq,Ord,Functor,Foldable,Traversable,Generic)

instance Eq1 Type where
  liftEq _ TyAny TyAny = True
  liftEq eq (TyVar a) (TyVar m) = liftEq eq a m
  liftEq _ (TyPrim a) (TyPrim m) = a == m
  liftEq eq (TyList a) (TyList m) = liftEq eq a m
  liftEq eq (TySchema a b) (TySchema m n) = a == m && liftEq eq b n
  liftEq eq (TyFun a) (TyFun b) = liftEq eq a b
  liftEq eq (TyUser a) (TyUser b) = eq a b
  liftEq _ _ _ = False

instance NFData v => NFData (Type v)

instance (Show v) => Show (Type v) where
  show (TyPrim t) = show t
  show (TyList t) | isAnyTy t = unpack tyList
                  | otherwise = "[" ++ show t ++ "]"
  show (TySchema s t) | isAnyTy t = show s
                      | otherwise = show s ++ ":" ++ show t
  show (TyFun f) = show f
  show (TyUser v) = show v
  show TyAny = "*"
  show (TyVar n) = show n

instance (Pretty o) => Pretty (Type o) where
  pretty ty = case ty of
    TyVar n -> pretty n
    TyUser v -> pretty v
    TyFun f -> pretty f
    TySchema s t -> pretty s PP.<> colon PP.<> pretty t
    TyList t -> "list:" PP.<> pretty t
    TyPrim t -> pretty t
    TyAny -> "*"

mkTyVar :: TypeVarName -> [Type n] -> Type n
mkTyVar n cs = TyVar (TypeVar n cs)
mkTyVar' :: TypeVarName -> Type n
mkTyVar' n = mkTyVar n []
mkSchemaVar :: TypeVarName -> Type n
mkSchemaVar n = TyVar (SchemaVar n)

isAnyTy :: Type v -> Bool
isAnyTy TyAny = True
isAnyTy _ = False

isVarTy :: Type v -> Bool
isVarTy TyVar {} = True
isVarTy _ = False

isUnconstrainedTy :: Type v -> Bool
isUnconstrainedTy TyAny = True
isUnconstrainedTy (TyVar (TypeVar _ [])) = True
isUnconstrainedTy _ = False
{-# INLINE isUnconstrainedTy #-}

-- | a `canUnifyWith` b means a "can represent/contains" b
canUnifyWith :: Eq n => Type n -> Type n -> Bool
canUnifyWith TyAny _ = True
canUnifyWith _ TyAny = True
canUnifyWith (TyVar (SchemaVar _)) TyUser {} = True
canUnifyWith (TyVar SchemaVar {}) (TyVar SchemaVar {}) = True
canUnifyWith (TyVar (TypeVar _ ac)) (TyVar (TypeVar _ bc)) = all (`elem` ac) bc
canUnifyWith (TyVar (TypeVar _ cs)) b = null cs || b `elem` cs
canUnifyWith (TyList a) (TyList b) = a `canUnifyWith` b
canUnifyWith (TySchema _ a) (TySchema _ b) = a `canUnifyWith` b
canUnifyWith a b = a == b
{-# INLINE canUnifyWith #-}

makeLenses ''Type
makeLenses ''FunType
makeLenses ''Arg
makeLenses ''TypeVar
makeLenses ''TypeVarName
