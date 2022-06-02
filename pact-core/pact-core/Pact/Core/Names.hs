{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
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
 , Supply
 , DeclName(..)
 , NamedDeBruijn(..)
 , ndIndex
 , ndName
 , DeBruijn(..)
 , TypeName(..)
 , rawParsedName
 , ONameKind
 , OverloadedName
 ) where

import Control.Lens
import Data.Text(Text)
import Data.Word(Word64)

import Pact.Core.Hash
import Pact.Core.Pretty(Pretty(..))


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

rawParsedName :: ParsedName -> Text
rawParsedName (BN (BareName n)) = n
rawParsedName (QN qn) = _qnName qn

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
  deriving (Num, Enum) via Int

type Supply = Unique

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
  deriving (Num, Bounded, Enum) via Word64

data DeclName
  = DeclName
  { _tlnHash :: ModuleHash
  , _tlnName :: !Text
  , _tlnModule :: !ModuleName
  }
  deriving (Show, Eq, Ord)

data ONameKind b
  = OBound DeBruijn
  | OTopLevel ModuleName ModuleHash
  | OBuiltinDict b
  deriving (Show, Eq)

data OverloadedName b
  = OverloadedName
  { _olName :: !Text
  , _olNameKind :: ONameKind b }
  deriving (Show, Eq)

-- Name representing locally nameless representations
data Name
  = Name
  { _nName :: !Text
  , _nKind :: NameKind }
  deriving (Show, Eq, Ord)

data NameKind
  = LocallyBoundName DeBruijn
  | TopLevelName ModuleName ModuleHash
  deriving (Show, Eq, Ord)

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
makeLenses ''NamedDeBruijn

instance Pretty Name where
  pretty (Name n nk) = case nk of
    LocallyBoundName _ ->
      pretty n
    _ -> undefined

instance Pretty NamedDeBruijn where
  pretty (NamedDeBruijn _ n) =
    pretty n
