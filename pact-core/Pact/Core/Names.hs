{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Names
 ( ModuleName(..)
 , NamespaceName(..)
 , Field(..)
 , IRNameKind(..)
 , ParsedName(..)
 , Name(..)
 , BareName(..)
 , IRName(..)
 , QualifiedName(..)
 , TypeVar(..)
 , Unique(..)
--  , nameRaw
--  , nameUnique
--  , nameKind
 , tyVarName
 , tyVarUnique
 , tyname
 , tynameUnique
 , newUnique
 , Supply(..)
 ) where

import Control.Lens
import Data.Text(Text)
import Data.IntMap.Strict(IntMap)
import Data.Int(Int32)
import Data.IORef (IORef, atomicModifyIORef')

import Pact.Core.Hash

import Data.Text.Prettyprint.Doc(Pretty(..))

newtype NamespaceName = NamespaceName { _namespaceName :: Text }
  deriving (Eq, Ord, Show)

instance Pretty NamespaceName where
  pretty (NamespaceName n) = pretty n

data ModuleName = ModuleName
  { _mnName      :: Text
  , _mnNamespace :: Maybe NamespaceName
  } deriving (Eq, Ord, Show)

instance Pretty ModuleName where
  pretty (ModuleName m mn) =
    maybe mempty (\b -> pretty b <> ".") mn <> pretty m

newtype BareName
  = BareName
  { _bnName :: Text }
  deriving (Show, Eq, Ord)

instance Pretty BareName where
  pretty (BareName b) = pretty b

data QualifiedName =
  QualifiedName
  { _qnName :: Text
  , _qnModName :: ModuleName
  } deriving (Show, Eq, Ord)

instance Pretty QualifiedName where
  pretty (QualifiedName n m) =
    pretty m <> "." <> pretty n

data ParsedName
  = QN QualifiedName
  | BN BareName
  deriving Show

instance Pretty ParsedName where
  pretty = \case
    QN qn -> pretty qn
    BN n -> pretty n

newtype Field = Field Text
  deriving (Eq, Ord, Show)

instance Pretty Field where
  pretty (Field f) = pretty f


-- Todo:
data IRNameKind
  = IRLocallyBoundName
  | IRTopLevelName ModuleName
  | IRModuleName
  deriving Show

data IRName
  = IRName
  { _irName :: !Text
  , _irNameKind :: IRNameKind
  , _irUnique :: Unique
  } deriving Show

-- Uniques
newtype Unique = Unique Int deriving (Show, Eq, Ord)

newtype Supply = Supply (IORef Int)

newtype ByUnique a = ByUnique a deriving (Show)

class HasUnique a where
  unique :: Lens' a Unique

instance HasUnique Unique where
  unique f u = f u

instance (HasUnique a) => Eq (ByUnique a) where
  (ByUnique l) == (ByUnique r) = view unique l == view unique r

instance (HasUnique a) => Ord (ByUnique a) where
  (ByUnique l) <= (ByUnique r) = view unique l <= view unique r

-- Unique Map
newtype UniqueMap a
  = UniqueMap (IntMap a)
  deriving stock (Show, Eq)
  deriving (Semigroup, Monoid) via (IntMap a)

type instance Index (UniqueMap a) = Unique
type instance IxValue (UniqueMap a) = a

instance Ixed (UniqueMap a) where
  ix (Unique i) f (UniqueMap m)= UniqueMap <$> ix i f m

instance At (UniqueMap a) where
  at (Unique i) f (UniqueMap m) =
    UniqueMap <$> at i f m

newUnique :: Supply -> IO Unique
newUnique (Supply ref) =
  atomicModifyIORef' ref $ \x ->
    let !z = x+1 in (z,Unique z)

data NamedDeBruijn
  = NamedDeBruijn
  { _ndIndex :: Int32
  , _ndName :: Text }
  deriving (Show, Eq, Ord)

data TopLevelName
  = TopLevelName
  { _tlnName :: Text
  , _tlnModule :: ModuleName
  , _tlnHash :: ModuleHash
  } deriving (Show, Eq)

data Name
  = TLN TopLevelName
  | DB NamedDeBruijn
  deriving (Show, Eq)

data TypeVar
  = TypeVar
  { _tyVarName :: !Text
  , _tyVarUnique :: !Unique }
  | UnificationVar
  { _tyVarName :: !Text
  , _tyVarUnique :: !Unique }
  deriving (Show, Eq)

data TypeName
  = TypeName
  { _tyname :: !Text
  , _tynameUnique :: !Unique }
  deriving (Show, Eq)

makeLenses ''TypeVar
makeLenses ''TypeName

instance HasUnique TypeVar where
  unique f = \case
    TypeVar a u -> TypeVar a <$> f u
    UnificationVar a u -> UnificationVar a <$> f u

instance HasUnique TypeName where
  unique = tynameUnique
