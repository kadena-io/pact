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
    TypeName(..)
  , Arg(..)
  , aInfo
  , aName
  , aType
  , FunType(..)
  , ftArgs
  , ftReturn
  , FunTypes
  , funTypes
  , prettyFunTypes
  , PrimType(..)
  , GuardType(..)
  , SchemaType(..)
  , SchemaPartial(..)
  , TypeVarName(..)
  , typeVarName
  , TypeVar(..)
  , tvName
  , tvConstraint
  , Type(..)
  , tyFunType
  , tyListType
  , tySchema
  , tySchemaType
  , tySchemaPartial
  , tyUser
  , tyVar
  , tyGuard
  , tyModSpecInterfaces
  , mkTyVar
  , mkTyVar'
  , mkSchemaVar
  , isAnyTy
  , isVarTy
  , canUnifyWith
  -- * String representations
  , tyInteger
  , tyDecimal
  , tyTime
  , tyBool
  , tyString
  , tyList
  , tyObject
  , tyKeySet
  , tyTable

  ) where

import Data.Eq.Deriving
import Text.Show.Deriving

import Control.Applicative
import Control.Lens hiding ((.=))
import Data.List
import Control.Monad
import Prelude
import Data.Aeson
import Data.String
import Data.Set (Set, isSubsetOf)
import qualified Data.Set as Set
import Data.Thyme.Format.Aeson ()
import GHC.Generics (Generic)
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Foldable
import Control.DeepSeq
import Data.Text (Text,unpack)
import Data.Default (Default(..))

import Pact.Types.Pretty
import Pact.Types.Util
import Pact.Types.Info
import Pact.Types.Codec


newtype TypeName = TypeName Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Pretty,Generic,NFData,Show)

-- | Pair a name and a type (arguments, bindings etc)
data Arg o = Arg {
  _aName :: Text,
  _aType :: Type o,
  _aInfo :: Info
  } deriving (Eq,Ord,Functor,Foldable,Traversable,Generic,Show)

instance NFData o => NFData (Arg o)
instance Pretty o => Pretty (Arg o) where
  pretty (Arg n t _) = pretty n <> colon <> pretty t
instance ToJSON o => ToJSON (Arg o) where toJSON = lensyToJSON 2
instance FromJSON o => FromJSON (Arg o) where parseJSON = lensyParseJSON 2
instance HasInfo (Arg o) where getInfo = _aInfo

-- | Function type
data FunType o = FunType {
  _ftArgs :: [Arg o],
  _ftReturn :: Type o
  } deriving (Eq,Ord,Functor,Foldable,Traversable,Generic,Show)

instance NFData o => NFData (FunType o)
instance (Pretty o) => Pretty (FunType o) where
  pretty (FunType as t) = hsep (map pretty as) <+> "->" <+> pretty t

instance ToJSON o => ToJSON (FunType o) where toJSON = lensyToJSON 3
instance FromJSON o => FromJSON (FunType o) where parseJSON = lensyParseJSON 3

-- | use NonEmpty for function types
type FunTypes o = NonEmpty (FunType o)

funTypes :: FunType o -> FunTypes o
funTypes ft = ft :| []

prettyFunTypes :: Pretty o => FunTypes o -> Doc
prettyFunTypes (t :| []) = pretty t
prettyFunTypes ts = prettyList (toList ts)

data GuardType
  = GTyKeySet
  | GTyKeySetName
  | GTyPact
  | GTyUser
  | GTyModule
  deriving (Eq,Ord,Generic,Show)

instance ToJSON GuardType where
  toJSON g = case g of
    GTyKeySet -> "keyset"
    GTyKeySetName -> "keysetref"
    GTyPact -> "pact"
    GTyUser -> "user"
    GTyModule -> "module"
instance FromJSON GuardType where
  parseJSON = withText "GuardType" $ \t -> case t of
    "keyset" -> pure GTyKeySet
    "keysetref" -> pure GTyKeySetName
    "pact" -> pure GTyPact
    "user" -> pure GTyUser
    "module" -> pure GTyModule
    _ -> fail "Unrecognized guard type"

instance NFData GuardType

-- | Primitive/unvarying types.
-- Guard is lame Maybe to allow "wildcards".
data PrimType =
  TyInteger |
  TyDecimal |
  TyTime |
  TyBool |
  TyString |
  TyGuard (Maybe GuardType)
  deriving (Eq,Ord,Generic,Show)

instance NFData PrimType
instance ToJSON PrimType where
  toJSON a = case a of
    TyInteger -> String tyInteger
    TyDecimal -> String tyDecimal
    TyTime -> String tyTime
    TyBool -> String tyBool
    TyString -> String tyString
    TyGuard g -> object [ "guard" .= g ]
instance FromJSON PrimType where
  parseJSON v = withText "PrimType" doStr v <|> withObject "PrimType" doObj v
    where
      doStr s
        | s == tyInteger = pure TyInteger
        | s == tyDecimal = pure TyDecimal
        | s == tyTime = pure TyTime
        | s == tyBool = pure TyBool
        | s == tyString = pure TyString
        | otherwise = fail "Bad PrimType Value"
      doObj o = TyGuard <$> o .: "guard"


tyInteger,tyDecimal,tyTime,tyBool,tyString,tyList,tyObject,
  tyKeySet,tyTable,tyGuard :: Text
tyInteger = "integer"
tyDecimal = "decimal"
tyTime = "time"
tyBool = "bool"
tyString = "string"
tyList = "list"
tyObject = "object"
tyKeySet = "keyset"
tyGuard = "guard"
tyTable = "table"

instance Pretty PrimType where
  pretty t = pretty $ case t of
    TyInteger -> tyInteger
    TyDecimal -> tyDecimal
    TyTime -> tyTime
    TyBool -> tyBool
    TyString -> tyString
    TyGuard tg -> case tg of
      Just GTyKeySet -> tyKeySet
      _ -> tyGuard

data SchemaType =
  TyTable |
  TyObject |
  TyBinding
  deriving (Eq,Ord,Generic,Show)

instance ToJSON SchemaType where
  toJSON TyTable = "table"
  toJSON TyObject = "object"
  toJSON TyBinding = "binding"
instance FromJSON SchemaType where
  parseJSON = withText "SchemaType" $ \t -> case t of
    "table" -> pure TyTable
    "object" -> pure TyObject
    "binding" -> pure TyBinding
    _ -> fail "Bad SchemaType value"

instance NFData SchemaType
instance Pretty SchemaType where
  pretty TyTable   = pretty tyTable
  pretty TyObject  = pretty tyObject
  pretty TyBinding = "binding"

newtype TypeVarName = TypeVarName { _typeVarName :: Text }
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Hashable,Pretty,Generic,NFData)

instance Show TypeVarName where show (TypeVarName t) = show t

-- | Type variables are namespaced for value types and schema types.
data TypeVar v =
  TypeVar { _tvName :: TypeVarName, _tvConstraint :: [Type v] } |
  SchemaVar { _tvName :: TypeVarName }
  deriving (Functor,Foldable,Traversable,Generic,Show)

instance ToJSON v => ToJSON (TypeVar v) where toJSON = lensyToJSON 3
instance FromJSON v => FromJSON (TypeVar v) where parseJSON = lensyParseJSON 3

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
instance (Pretty v) => Pretty (TypeVar v) where
  pretty (TypeVar n []) = angles $ pretty n
  pretty (TypeVar n cs) = angles $ pretty n <> commaBrackets (map pretty cs)
  pretty (SchemaVar n)  = angles $ braces $ pretty n

-- | Represent a full or partial schema inhabitant.
--
-- @PartialSchema@ represents a schema with only the given subset of fields.
-- @AnySubschema@ is used in places where any subset of fields can be provided.
-- @AnySubschema@ unifies with any larger type to give that type.
data SchemaPartial = FullSchema | PartialSchema !(Set Text) | AnySubschema
  deriving (Eq,Ord,Show,Generic)
instance NFData SchemaPartial
instance Default SchemaPartial where def = FullSchema
instance Pretty SchemaPartial where
  pretty (PartialSchema s) = "PartialSchema " <> pretty (Set.toList s)
  pretty sp = viaShow sp

instance ToJSON SchemaPartial where
  toJSON FullSchema = "full"
  toJSON AnySubschema = "any"
  toJSON (PartialSchema s) = toJSON s
instance FromJSON SchemaPartial where
  parseJSON v =
    (withThisText "FullSchema" "full" v $ pure FullSchema) <|>
    (withThisText "AnySubschema" "any" v $ pure AnySubschema) <|>
    (PartialSchema <$> parseJSON v)


showPartial :: SchemaPartial -> String
showPartial FullSchema = ""
showPartial (PartialSchema ks)
  = "~[" ++ intercalate "," (unpack.asString <$> Set.toList ks) ++ "]"
showPartial AnySubschema = "~"

-- | Pact types.
data Type v =
  TyAny |
  TyVar { _tyVar :: TypeVar v } |
  TyPrim PrimType |
  TyList { _tyListType :: Type v } |
  TySchema
  { _tySchema :: SchemaType
  , _tySchemaType :: Type v
  , _tySchemaPartial :: SchemaPartial } |
  TyFun { _tyFunType :: FunType v } |
  TyUser { _tyUser :: v } |
  TyModSpec
  { _tyModSpecInterfaces :: [v] }
    deriving (Eq,Ord,Functor,Foldable,Traversable,Generic,Show)

instance NFData v => NFData (Type v)

instance (Pretty o) => Pretty (Type o) where
  pretty ty = case ty of
    TyVar n        -> pretty n
    TyUser v       -> pretty v
    TyFun f        -> pretty f
    TySchema s t p -> pretty s <> colon <> prettyList (showPartial p) <> pretty t
    TyList t       -> brackets $ pretty t
    TyPrim t       -> pretty t
    TyModSpec is    -> "module{" <> prettyList (is) <> "}"
    TyAny          -> "*"

instance ToJSON v => ToJSON (Type v) where
  toJSON t = case t of
    TyAny -> "*"
    TyVar n -> toJSON n
    TyPrim pt -> toJSON pt
    TyList l -> object [ "list" .= l ]
    TySchema st ty p -> object [ "schema" .= st, "type" .= ty, "partial" .= p ]
    TyFun f -> toJSON f
    TyUser v -> toJSON v
    TyModSpec is -> object [ "modspec" .= is ]

instance FromJSON v => FromJSON (Type v) where
  parseJSON v =
    (withThisText "TyAny" "*" v $ pure TyAny) <|>
    (TyVar <$> parseJSON v) <|>
    (TyPrim <$> parseJSON v) <|>
    (TyFun <$> parseJSON v) <|>
    (TyUser <$> parseJSON v) <|>
    (withObject "TyList"
      (\o -> TyList <$> o .: "list") v) <|>
    (withObject "TySchema"
      (\o -> TySchema
        <$> o .: "schema"
        <*> o .: "type"
        <*> o .: "partial")
      v) <|>
    (withObject "TyModSpec"
      (\o -> TyModSpec <$> o .: "modspec") v)




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

-- | Used by runtime typechecking to unify a specification with a value,
-- 'a canUnifyWith b' means 'a' "can represent/contains" 'b'.
-- In use, 'a' is the spec, and 'b' is the value type being unified with 'a'.
canUnifyWith :: Eq n => Type n -> Type n -> Bool
canUnifyWith a b | a == b = True
canUnifyWith TyAny _ = True
canUnifyWith _ TyAny = False
canUnifyWith (TyVar (SchemaVar _)) TyUser {} = True
canUnifyWith (TyVar SchemaVar {}) (TyVar SchemaVar {}) = True
canUnifyWith (TyVar (TypeVar _ ac)) (TyVar (TypeVar _ bc)) = all (`elem` ac) bc
canUnifyWith (TyVar (TypeVar _ cs)) b = null cs || b `elem` cs
canUnifyWith (TyList a) (TyList b) = a `canUnifyWith` b
canUnifyWith (TySchema _ aTy aP) (TySchema _ bTy bP)
  = aTy `canUnifyWith` bTy && bP `isSubPartial` aP
canUnifyWith (TyPrim (TyGuard a)) (TyPrim (TyGuard b)) = case (a,b) of
  (Nothing,Just _) -> True
  (Just _,Nothing) -> True
  _ -> a == b
canUnifyWith (TyModSpec a) (TyModSpec b) = all (`elem` b) a
canUnifyWith _ _ = False
{-# INLINE canUnifyWith #-}

-- | @a `isSubPartial` b@ means that @a <= b@ in the lattice given by
-- @SchemaPartial@, ie, that @a@ is smaller than @b@.
isSubPartial :: SchemaPartial -> SchemaPartial -> Bool
isSubPartial _ FullSchema = True
isSubPartial FullSchema _ = False
isSubPartial AnySubschema _ = True
isSubPartial _ AnySubschema = False
isSubPartial (PartialSchema a) (PartialSchema b) = a `isSubsetOf` b




makeLenses ''Type
makeLenses ''FunType
makeLenses ''Arg
makeLenses ''TypeVar
makeLenses ''TypeVarName

deriveShow1 ''TypeVar
deriveShow1 ''Arg
deriveShow1 ''FunType
deriveShow1 ''Type
deriveEq1 ''TypeVar
deriveEq1 ''Arg
deriveEq1 ''FunType
deriveEq1 ''Type
