{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
  , _TyAny, _TyVar, _TyPrim
  , _TyList, _TySchema, _TyFun
  , _TyUser, _TyModule
  , tyFunType
  , tyListType
  , tySchema
  , tySchemaType
  , tySchemaPartial
  , tyUser
  , tyVar
  , tyGuard
  , tyModuleSpec
  , mkTyVar
  , mkTyVar'
  , mkSchemaVar
  , isAnyTy
  , isVarTy
  , unifiesWith
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
  , argEq1
  ) where


import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding ((.=), elements)
import Control.Monad
import Data.Aeson
import Data.Default (Default(..))
import Data.Eq.Deriving
import Data.Foldable
import Data.Functor.Classes
import Data.Hashable
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set, isSubsetOf)
import qualified Data.Set as Set
import Data.String
import Data.Text (Text,unpack)
import GHC.Generics (Generic)
import Prelude
import Text.Show.Deriving

import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Pact.Types.Codec
import Pact.Types.Info
import Pact.Types.Pretty
import Pact.Types.Util
import Pact.Types.SizeOf


newtype TypeName = TypeName Text
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Pretty,Generic,NFData,Show)

instance SizeOf TypeName where
  sizeOf (TypeName n) = sizeOf n

instance Arbitrary TypeName where
  arbitrary = TypeName <$> arbitrary

-- | Pair a name and a type (arguments, bindings etc)
data Arg o = Arg {
  _aName :: Text,
  _aType :: Type o,
  _aInfo :: Info
  } deriving (Eq,Ord,Functor,Foldable,Traversable,Generic,Show)

instance NFData o => NFData (Arg o)
instance Pretty o => Pretty (Arg o) where
  pretty (Arg n t _) = pretty n <> colon <> pretty t
instance HasInfo (Arg o) where getInfo = _aInfo

argProperties :: ToJSON o => JsonProperties (Arg o)
argProperties o =
  [ "name" .= _aName o
  , "type" .= _aType o
  , "info" .= _aInfo o
  ]
{-# INLINE argProperties #-}

instance ToJSON o => ToJSON (Arg o) where
  toJSON = enableToJSON "Pact.Types.Type.Arg" . object . argProperties
  toEncoding = pairs . mconcat . argProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON o => FromJSON (Arg o) where
  parseJSON = withObject "Arg" $ \o -> Arg
    <$> o .: "name"
    <*> o .: "type"
    <*> o .: "info"
  {-# INLINE parseJSON #-}

instance (SizeOf o) => SizeOf (Arg o)

instance Arbitrary o => Arbitrary (Arg o) where
  arbitrary = Arg <$> arbitrary <*> arbitrary <*> arbitrary

-- | Function type
data FunType o = FunType {
  _ftArgs :: [Arg o],
  _ftReturn :: Type o
  } deriving (Eq,Ord,Functor,Foldable,Traversable,Generic,Show)

instance NFData o => NFData (FunType o)
instance (Pretty o) => Pretty (FunType o) where
  pretty (FunType as t) = hsep (map pretty as) <+> "->" <+> pretty t

funTypeProperties :: ToJSON o => JsonProperties (FunType o)
funTypeProperties o =
  [ "args" .= _ftArgs o
  , "return" .= _ftReturn o
  ]
{-# INLINE funTypeProperties #-}

instance ToJSON o => ToJSON (FunType o) where
  toJSON = enableToJSON "Pact.Types.Type.FunType" . object . funTypeProperties
  toEncoding = pairs . mconcat . funTypeProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON o => FromJSON (FunType o) where
  parseJSON = withObject "FunType" $ \o -> FunType
    <$> o .: "args"
    <*> o .: "return"
  {-# INLINE parseJSON #-}

instance (SizeOf o) => SizeOf (FunType o)

instance Arbitrary o => Arbitrary (FunType o) where
  -- Let's keep the number of arguments small
  arbitrary = do
    Positive k <- arbitrary
    scale (`div` (k + 1)) $ FunType <$> (take 5 <$> arbitrary) <*> arbitrary

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
  | GTyCapability
  deriving (Eq,Ord,Generic,Show)

instance ToJSON GuardType where
  toJSON = enableToJSON "Pact.Types.Type.GuardType" . \case
    GTyKeySet -> "keyset"
    GTyKeySetName -> "keysetref"
    GTyPact -> "pact"
    GTyUser -> "user"
    GTyModule -> "module"
    GTyCapability -> "capability"
  toEncoding = \case
    GTyKeySet -> toEncoding @Text "keyset"
    GTyKeySetName -> toEncoding @Text "keysetref"
    GTyPact -> toEncoding @Text "pact"
    GTyUser -> toEncoding @Text "user"
    GTyModule -> toEncoding @Text "module"
    GTyCapability -> toEncoding @Text "capability"
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON GuardType where
  parseJSON = withText "GuardType" $ \case
    "keyset" -> pure GTyKeySet
    "keysetref" -> pure GTyKeySetName
    "pact" -> pure GTyPact
    "user" -> pure GTyUser
    "module" -> pure GTyModule
    "capability" -> pure GTyCapability
    _ -> fail "Unrecognized guard type"
  {-# INLINE parseJSON #-}

instance NFData GuardType

instance Arbitrary GuardType where
  arbitrary = elements [ GTyKeySet, GTyKeySetName, GTyPact, GTyUser, GTyModule ]

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
  toJSON = enableToJSON "Pact.Types.Type.PrimType" . \case
    TyInteger -> String tyInteger
    TyDecimal -> String tyDecimal
    TyTime -> String tyTime
    TyBool -> String tyBool
    TyString -> String tyString
    TyGuard g -> object [ "guard" .= g ]
  toEncoding = \case
    TyInteger -> toEncoding tyInteger
    TyDecimal -> toEncoding tyDecimal
    TyTime -> toEncoding tyTime
    TyBool -> toEncoding tyBool
    TyString -> toEncoding tyString
    TyGuard g -> pairs ("guard" .= g)
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

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
  {-# INLINE parseJSON #-}

instance SizeOf PrimType where
  sizeOf _ = 0

instance Arbitrary PrimType where
  arbitrary = oneof
    [ pure TyInteger
    , pure TyDecimal
    , pure TyTime
    , pure TyBool
    , pure TyString
    , TyGuard <$> arbitrary
    ]

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
  toJSON = enableToJSON "Pact.Types.Type.SchemaType" . \case
    TyTable -> "table"
    TyObject -> "object"
    TyBinding -> "binding"

  toEncoding TyTable = toEncoding @Text "table"
  toEncoding TyObject = toEncoding @Text "object"
  toEncoding TyBinding = toEncoding @Text "binding"

  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON SchemaType where
  parseJSON = withText "SchemaType" $ \case
    "table" -> pure TyTable
    "object" -> pure TyObject
    "binding" -> pure TyBinding
    _ -> fail "Bad SchemaType value"
  {-# INLINE parseJSON #-}

instance NFData SchemaType
instance Pretty SchemaType where
  pretty TyTable   = pretty tyTable
  pretty TyObject  = pretty tyObject
  pretty TyBinding = "binding"

instance SizeOf SchemaType where
  sizeOf _ = 0

instance Arbitrary SchemaType where
  arbitrary = elements [TyTable, TyObject, TyBinding]

newtype TypeVarName = TypeVarName { _typeVarName :: Text }
  deriving (Eq,Ord,IsString,AsString,ToJSON,FromJSON,Hashable,Pretty,Generic,NFData)

instance Arbitrary TypeVarName where
  arbitrary = TypeVarName <$> arbitrary

instance Show TypeVarName where show (TypeVarName t) = show t

instance SizeOf TypeVarName where
  sizeOf = sizeOf . _typeVarName

-- | Type variables are namespaced for value types and schema types.
data TypeVar v =
  TypeVar { _tvName :: TypeVarName, _tvConstraint :: [Type v] } |
  SchemaVar { _tvName :: TypeVarName }
  deriving (Functor,Foldable,Traversable,Generic,Show)

typeVarProperties :: ToJSON v => JsonProperties (TypeVar v)
typeVarProperties o@TypeVar{} =
  [ "tag" .= ("TypeVar" :: Text)
  , "name" .= _tvName o
  , "constraint" .= _tvConstraint o
  ]
typeVarProperties o@SchemaVar{} =
  [ "tag" .= ("SchemaVar" :: Text)
  , "name" .= _tvName o
  ]

instance ToJSON v => ToJSON (TypeVar v) where
  toJSON = enableToJSON "Pact.Types.Type.TypeVar" . object . typeVarProperties
  toEncoding = pairs . mconcat . typeVarProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON v => FromJSON (TypeVar v) where
  parseJSON = withObject "TypeVar" $ \o -> (o .: "tag") >>= \case
    ("TypeVar" :: Text) -> TypeVar <$> o .: "name" <*> o .: "constraint"
    "SchemaVar" -> SchemaVar <$> o .: "name"
    t -> fail ("unexpected constructor tag: " <> unpack t)
  {-# INLINE parseJSON #-}

instance Arbitrary v => Arbitrary (TypeVar v) where
  arbitrary = oneof
    [ TypeVar <$> arbitrary <*> arbitrary
    , SchemaVar <$> arbitrary
    ]

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

instance (SizeOf v) => SizeOf (TypeVar v)

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
  toJSON = enableToJSON "Pact.Types.Type.SchemaPartial" . \case
    FullSchema -> "full"
    AnySubschema -> "any"
    (PartialSchema s) -> toJSON s
  toEncoding = \case
    FullSchema -> toEncoding @Text "full"
    AnySubschema -> toEncoding @Text "any"
    (PartialSchema s) -> toEncoding s
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON SchemaPartial where
  parseJSON v =
    withThisText "FullSchema" "full" v (pure FullSchema) <|>
    withThisText "AnySubschema" "any" v (pure AnySubschema) <|>
    (PartialSchema <$> parseJSON v)
  {-# INLINE parseJSON #-}

instance SizeOf SchemaPartial

instance Arbitrary SchemaPartial where
  arbitrary = oneof
    [ pure FullSchema
    , PartialSchema <$> arbitrary
    , pure AnySubschema
    ]

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
  TyModule
  { _tyModuleSpec :: Maybe [v]
    -- ^ Nothing for interfaces, implemented ifaces for modules
  }
    deriving (Eq,Ord,Functor,Foldable,Traversable,Generic,Show)

instance SizeOf v => SizeOf (Type v)

instance Arbitrary v => Arbitrary (Type v) where
  arbitrary = sized $ \case
    0 -> oneof [ pure TyAny, TyPrim <$> arbitrary ]
    s -> do
      Positive k <- arbitrary
      resize (s `div` (k + 1)) $ oneof
        [ pure TyAny
        , TyVar <$> arbitrary
        , TyPrim <$> arbitrary
        , TyList <$> arbitrary
        , TySchema <$> arbitrary <*> arbitrary <*> arbitrary
        , TyFun <$> arbitrary
        , TyUser <$> arbitrary
        , TyModule <$> (fmap (take 5) <$> scale (`div` 2) arbitrary)
        ]

instance NFData v => NFData (Type v)

instance (Pretty o) => Pretty (Type o) where
  pretty ty = case ty of
    TyVar n        -> pretty n
    TyUser v       -> pretty v
    TyFun f        -> pretty f
    TySchema s t p -> pretty s <> colon <> prettyList (showPartial p) <> pretty t
    TyList t       -> brackets $ pretty t
    TyPrim t       -> pretty t
    TyModule is    -> "module" <> (maybe "" commaBraces' is)
    TyAny          -> "*"


instance ToJSON v => ToJSON (Type v) where
  toJSON = enableToJSON "Pact.Types.Type.Type" . \case
    TyAny -> "*"
    TyVar n -> toJSON n
    TyPrim pt -> toJSON pt
    TyList l -> object [ "list" .= l ]
    TySchema st ty p -> object [ "schema" .= st, "type" .= ty, "partial" .= p ]
    TyFun f -> toJSON f
    TyUser v -> toJSON v
    TyModule is -> object [ "modspec" .= is ]
  toEncoding t = case t of
    TyAny -> toEncoding @Text "*"
    TyVar n -> toEncoding n
    TyPrim pt -> toEncoding pt
    TyList l -> pairs ("list" .= l)
    TySchema st ty p -> pairs $ mconcat [ "schema" .= st, "type" .= ty, "partial" .= p ]
    TyFun f -> toEncoding f
    TyUser v -> toEncoding v
    TyModule is -> pairs ("modspec" .= is)
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON v => FromJSON (Type v) where
  parseJSON v =
    withThisText "TyAny" "*" v (pure TyAny) <|>
    (TyVar <$> parseJSON v) <|>
    (TyPrim <$> parseJSON v) <|>
    (TyFun <$> parseJSON v) <|>
    withObject "TyList" (\o -> TyList <$> o .: "list") v <|>
    withObject "TySchema"
      (\o -> TySchema
        <$> o .: "schema"
        <*> o .: "type"
        <*> o .: "partial")
      v <|>
    withObject "TyModule" (\o -> TyModule <$> o .: "modspec") v <|>
    (TyUser <$> parseJSON v)
  {-# INLINE parseJSON #-}

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
-- 'a unifiesWith b' means 'a' "can represent/contains" 'b'.
-- In use, 'a' is the spec, and 'b' is the value type being unified with 'a'.
-- First argument is 'Eq1' predicate.
unifiesWith :: Eq1 Type => (n -> n -> Bool) -> Type n -> Type n -> Bool
unifiesWith f a b | liftEq f a b = True
unifiesWith _ TyAny _ = True
unifiesWith _ _ TyAny = False
unifiesWith _ (TyVar (SchemaVar _)) TyUser {} = True
unifiesWith _ (TyVar SchemaVar {}) (TyVar SchemaVar {}) = True
unifiesWith f (TyVar (TypeVar _ ac)) (TyVar (TypeVar _ bc)) = all (\b -> elem1 f b ac) bc
unifiesWith f (TyVar (TypeVar _ cs)) b = null cs || elem1 f b cs
unifiesWith f (TyList a) (TyList b) = unifiesWith f a b
unifiesWith f (TySchema _ aTy aP) (TySchema _ bTy bP)
  = unifiesWith f aTy bTy && bP `isSubPartial` aP
unifiesWith _ (TyPrim (TyGuard a)) (TyPrim (TyGuard b)) = case (a,b) of
  (Nothing,Just _) -> True
  (Just _,Nothing) -> True
  _ -> a == b
unifiesWith f (TyModule a) (TyModule b) =
  liftEq (\x y -> all (\xe -> elem' f xe y) x) a b
unifiesWith _ _ _ = False
{-# INLINE unifiesWith #-}

-- | @a `isSubPartial` b@ means that @a <= b@ in the lattice given by
-- @SchemaPartial@, ie, that @a@ is smaller than @b@.
isSubPartial :: SchemaPartial -> SchemaPartial -> Bool
isSubPartial _ FullSchema = True
isSubPartial FullSchema _ = False
isSubPartial AnySubschema _ = True
isSubPartial _ AnySubschema = False
isSubPartial (PartialSchema a) (PartialSchema b) = a `isSubsetOf` b

elem1 :: (Foldable t, Eq1 f) =>
         (a -> a -> Bool) -> f a -> t (f a) -> Bool
elem1 f = elem' (liftEq f)

elem' :: Foldable t => (a -> a -> Bool) -> a -> t a -> Bool
elem' f = any . f

makeLenses ''Type
makePrisms ''Type
makeLenses ''FunType
makeLenses ''Arg
makeLenses ''TypeVar
makeLenses ''TypeVarName

instance Show1 Type where
  liftShowsPrec = $(makeLiftShowsPrec ''Type)
instance Show1 TypeVar where
  liftShowsPrec = $(makeLiftShowsPrec ''TypeVar)
instance Show1 FunType where
  liftShowsPrec = $(makeLiftShowsPrec ''FunType)
instance Show1 Arg where
  liftShowsPrec = $(makeLiftShowsPrec ''Arg)

instance Eq1 Type where
    liftEq = $(makeLiftEq ''Type)
instance Eq1 TypeVar where
    liftEq = $(makeLiftEq ''TypeVar)
instance Eq1 FunType where
    liftEq = $(makeLiftEq ''FunType)
instance Eq1 Arg where
    liftEq = $(makeLiftEq ''Arg)

argEq1 :: (n -> n -> Bool) -> Arg n -> Arg n -> Bool
argEq1 eq (Arg n t _) (Arg n' t' _) =
  n == n' && liftEq eq t t'
