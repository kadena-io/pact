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
 , NameKind(..)
 , BareName(..)
 , IRName(..)
 , irName
 , irNameKind
 , irUnique
 , QualifiedName(..)
 , TypeVar(..)
 , Unique(..)
 , tyVarName
 , tyVarUnique
 , tyname
 , tynameUnique
 , Supply(..)
 , newSupply
 , newUnique
 , DeclName(..)
 , NamedDeBruijn(..)
 , DeBruijn(..)
 ) where

import Control.Lens
import Data.Text(Text)
import Data.Word(Word64)
import Data.IORef
import Control.Monad.IO.Class(MonadIO(..))

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


-- Uniques
newtype Unique =
  Unique { _unique :: Int }
  deriving (Show, Eq, Ord)
  deriving Num via Int

newtype Supply = Supply (IORef Int)

newSupply :: MonadIO m => m Supply
newSupply = liftIO (Supply <$> newIORef 0)

newUnique :: MonadIO m => Supply -> m Unique
newUnique (Supply ref) = liftIO $ do
  i <- readIORef ref
  modifyIORef ref (+ 1)
  pure (Unique i)

-- Todo:
data IRNameKind
  = IRLocallyBoundName
  | IRTopLevelName ModuleName
  | IRModuleName
  deriving (Show)

data IRName
  = IRName
  { _irName :: !Text
  , _irNameKind :: IRNameKind
  , _irUnique :: Unique
  } deriving (Show)

makeLenses ''IRName

instance Eq IRName where
  l == r = _irUnique l == _irUnique r

instance Ord IRName where
  l <= r =_irUnique l <= _irUnique r

data NamedDeBruijn
  = NamedDeBruijn
  { _ndIndex :: !DeBruijn
  , _ndName :: Text }
  deriving (Show, Eq)

newtype DeBruijn
  = DeBruijn { _debruijn :: Word64 }
  deriving (Show, Eq, Ord)

data DeclName
  = DeclName
  { _tlnHash :: ModuleHash
  , _tlnName :: !Text
  , _tlnModule :: !ModuleName
  }
  deriving (Show, Eq, Ord)

-- Name representing locally nameless representations
data Name
  = Name
  { _nName :: !Text
  , _nKind :: NameKind }
  deriving (Show, Eq)

data NameKind
  = LocallyBoundName DeBruijn
  | TopLevelName ModuleName ModuleHash
  deriving (Show, Eq)

data TypeVar
  = TypeVar
  { _tyVarName :: !Text
  , _tyVarUnique :: !Unique }
  | UnificationVar
  { _tyVarName :: !Text
  , _tyVarUnique :: !Unique }
  deriving (Show)

instance Eq TypeVar where
  l == r = _tyVarUnique l == _tyVarUnique r

instance Ord TypeVar where
  l <= r = _tyVarUnique l <= _tyVarUnique r

data TypeName
  = TypeName
  { _tyname :: !Text
  , _tynameUnique :: !Unique }
  deriving (Show, Eq)

makeLenses ''TypeVar
makeLenses ''TypeName
