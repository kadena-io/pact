{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}



module Pact.Core.Names where

import Control.Lens
import Data.Text(Text)
import Data.IntMap.Strict(IntMap)
import Data.IORef (IORef, atomicModifyIORef')


newtype Field = Field Text
  deriving (Eq, Ord, Show)

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
    let z = x+1 in (z,Unique z)

data NameKind
  = TopLevelName -- Script top level names, not bound to any module
  | ModuleName
  | DefName
  | LocallyBoundName
  deriving (Show, Eq)

data Name
  = Name
  { _nameRaw :: !Text
  , _nameUnique :: !Unique
  , _nameKind :: NameKind }
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

makeLenses ''Name
makeLenses ''TypeVar
makeLenses ''TypeName

instance HasUnique Name where
  unique = nameUnique

instance HasUnique TypeVar where
  unique f = \case
    TypeVar a u -> TypeVar a <$> f u
    UnificationVar a u -> UnificationVar a <$> f u

instance HasUnique TypeName where
  unique = tynameUnique
