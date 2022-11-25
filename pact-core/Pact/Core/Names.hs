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
 , renderQualName
 , renderModuleName
 , TypeVar(..)
 , Unique
 , tyVarName
 , tyVarUnique
 , tyname
 , tynameUnique
 , Supply
 , NamedDeBruijn(..)
 , ndIndex
 , ndName
 , DeBruijn
 , TypeName(..)
 , rawParsedName
 , ONameKind(..)
 , OverloadedName(..)
 , FullyQualifiedName(..)
 , TableName(..)
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
  } deriving (Show, Eq)

instance Ord QualifiedName where
  compare (QualifiedName qn1 m1) (QualifiedName qn2 m2) =
    case compare m1 m2 of
      EQ -> compare qn1 qn2
      t -> t

renderQualName :: QualifiedName -> Text
renderQualName (QualifiedName n (ModuleName m ns)) =
  maybe "" ((<> ".") . _namespaceName) ns <> m <> "." <> n

renderModuleName :: ModuleName -> Text
renderModuleName (ModuleName m ns) =
  maybe "" ((<> ".") . _namespaceName) ns <> m

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
type Unique = Int
type Supply = Int

data IRNameKind
  = IRBound
  | IRTopLevel ModuleName ModuleHash
  deriving (Show, Eq, Ord)

data IRName
  = IRName
  { _irName :: !Text
  , _irNameKind :: IRNameKind
  , _irUnique :: Unique
  } deriving (Show, Eq, Ord)

makeLenses ''IRName

data NamedDeBruijn
  = NamedDeBruijn
  { _ndIndex :: !DeBruijn
  , _ndName :: Text }
  deriving (Show, Eq)

type DeBruijn = Word64

data ONameKind b
  = OBound Unique
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
  = NBound DeBruijn
  | NTopLevel ModuleName ModuleHash
  deriving (Show, Eq, Ord)

data FullyQualifiedName
  = FullyQualifiedName
  { _fqModule :: ModuleName
  , _fqName :: !Text
  , _fqHash :: ModuleHash
  } deriving (Eq, Show, Ord)

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

instance Pretty TypeVar where
  pretty t = pretty (_tyVarName t)

data TypeName
  = TypeName
  { _tyname :: !Text
  , _tynameUnique :: !Unique }
  deriving (Show, Eq)

makeLenses ''TypeVar
makeLenses ''TypeName
makeLenses ''NamedDeBruijn

instance (Pretty b) => Pretty (OverloadedName b) where
  pretty (OverloadedName n nk) = case nk of
    OBound _ -> pretty n
    OBuiltinDict b -> "DICT<" <> pretty b <> ">"
    OTopLevel mn _ -> pretty mn <> "." <> pretty n

instance Pretty IRName where
  pretty r = pretty (_irName r)

instance Pretty Name where
  pretty (Name n nk) = case nk of
    NBound dix -> pretty n <> "<" <> pretty dix <> ">"
    NTopLevel mn _mh -> pretty mn <> "." <> pretty n

instance Pretty NamedDeBruijn where
  pretty (NamedDeBruijn _i _n) =
    pretty _n

newtype TableName = TableName Text
  deriving (Eq, Show)
