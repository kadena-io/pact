{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


-- |
-- Module      :  Pact.Types.Names
-- Copyright   :  (C) 2019 Stuart Popejoy, Kadena LLC
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Symbol name types.
--

module Pact.Types.Names
  ( NamespaceName(..)
  , NativeDefName(..)
  , DefName(..)
  , TableName(..)
  , ModuleName(..), mnName, mnNamespace, parseModuleName
  , Name(..), parseName
  , QualifiedName(..), parseQualifiedName
  , DynamicName(..)
  , BareName(..)
  ) where


import Control.Applicative
import Control.DeepSeq
import Control.Lens (makeLenses)
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import qualified Data.Attoparsec.Text as AP
import Data.Default
import Data.Hashable
import Data.Set (Set)
import Data.String
import Data.Text (Text,pack)
import qualified Data.Text as T

import GHC.Generics (Generic)

import Test.QuickCheck
import Text.Trifecta (ident,TokenParsing,(<?>),dot,eof)


import Pact.Types.Info
import Pact.Types.Parser
import Pact.Types.Pretty hiding (dot)
import Pact.Types.SizeOf
import Pact.Types.Util


newtype NamespaceName = NamespaceName Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON, IsString, AsString, Hashable, Pretty, Generic, NFData, SizeOf)

instance Arbitrary NamespaceName where
  arbitrary = NamespaceName <$> genBareText


data ModuleName = ModuleName
  { _mnName      :: Text
  , _mnNamespace :: Maybe NamespaceName
  } deriving (Eq, Ord, Generic, Show)

instance Arbitrary ModuleName where
  -- assumes most modules names defined inside a namespace
  arbitrary = ModuleName <$> genBareText <*> frequency
    [ (4, Just <$> arbitrary)
    , (1, pure Nothing) ]
instance Hashable ModuleName where
  hashWithSalt s (ModuleName n Nothing)   =
    s `hashWithSalt` (0::Int) `hashWithSalt` n
  hashWithSalt s (ModuleName n (Just ns)) =
    s `hashWithSalt` (1::Int) `hashWithSalt` n `hashWithSalt` ns

instance SizeOf ModuleName where
  sizeOf (ModuleName mn namespace) =
    (constructorCost 2) + (sizeOf mn) + (sizeOf namespace)

instance NFData ModuleName

instance AsString ModuleName where
  asString (ModuleName n Nothing) = n
  asString (ModuleName n (Just (NamespaceName ns))) = ns <> "." <> n

instance IsString ModuleName where
  fromString = coalesce . T.splitOn "." . pack
    where
      coalesce l = case l of
        [ns,n] -> ModuleName n (Just (NamespaceName ns))
        [n]    -> ModuleName n Nothing
        _      -> ModuleName (pack . show $ l) (Just . NamespaceName $ "Err: malformed name")

instance Pretty ModuleName where
  pretty (ModuleName n Nothing)   = pretty n
  pretty (ModuleName n (Just ns)) = pretty ns <> "." <> pretty n

instance ToJSON ModuleName where toJSON = lensyToJSON 3
instance FromJSON ModuleName where parseJSON = lensyParseJSON 3

moduleNameParser :: (TokenParsing m, Monad m) => m ModuleName
moduleNameParser = do
  a <- ident style
  b <- optional (dot *> ident style)
  case b of
    Nothing -> return (ModuleName a Nothing) <?> "module name"
    Just b' -> return (ModuleName b' (Just . NamespaceName $ a)) <?> "namespaced module name"

parseModuleName :: Text -> Either String ModuleName
parseModuleName = AP.parseOnly (moduleNameParser <* eof)


newtype DefName = DefName Text
    deriving (Eq,Ord,IsString,ToJSON,FromJSON,AsString,Hashable,Pretty,Show,NFData)


data QualifiedName = QualifiedName
  { _qnQual :: ModuleName
  , _qnName :: Text
  , _qnInfo :: Info
  } deriving (Generic,Show)

instance Arbitrary QualifiedName where
  arbitrary = QualifiedName <$> arbitrary <*> genBareText <*> arbitrary
instance Eq QualifiedName where
  (QualifiedName a b _c) == (QualifiedName d e _f) =
    a == d && b == e
instance Ord QualifiedName where
  (QualifiedName a b _c) `compare` (QualifiedName d e _f) =
    (a,b) `compare` (d,e)
instance Pretty QualifiedName where
  pretty QualifiedName{..} = pretty _qnQual <> "." <> pretty _qnName
instance HasInfo QualifiedName where getInfo = _qnInfo
instance NFData QualifiedName
instance AsString QualifiedName where asString = renderCompactText

instance SizeOf QualifiedName where
  sizeOf (QualifiedName modName n i) =
    (constructorCost 3) + (sizeOf modName) + (sizeOf n) + (sizeOf i)

instance ToJSON QualifiedName where
  toJSON = toJSON . renderCompactString

instance FromJSON QualifiedName where
  parseJSON = withText "QualifiedName" $ \t -> case parseQualifiedName def t of
    Left s  -> fail s
    Right n -> return n

qualifiedNameParser :: (TokenParsing m, Monad m) => Info -> m QualifiedName
qualifiedNameParser i = do
  a <- ident style
  b <- dot *> ident style
  c <- optional (dot *> ident style)
  case c of
    Nothing -> return (QualifiedName (ModuleName a Nothing) b i) <?> "qualified name"
    Just c' -> return (QualifiedName (ModuleName b (Just . NamespaceName $ a)) c' i) <?> "namespaced qualified name"

parseQualifiedName :: Info -> Text -> Either String QualifiedName
parseQualifiedName i = AP.parseOnly (qualifiedNameParser i <* eof)


data BareName = BareName
  { _bnName :: Text
  , _bnInfo :: Info
  } deriving (Generic,Eq,Show)
instance Arbitrary BareName where
  arbitrary = BareName <$> genBareText <*> arbitrary
instance Pretty BareName where
  pretty BareName{..} = pretty _bnName
instance HasInfo BareName where getInfo = _bnInfo
instance NFData BareName
instance AsString BareName where asString = renderCompactText

instance SizeOf BareName where
  sizeOf (BareName n i) =
    (constructorCost 2) + (sizeOf n) + (sizeOf i)

data DynamicName = DynamicName
    { _dynMember :: Text
    , _dynRefArg :: Text
    , _dynInterfaces :: Set ModuleName
    , _dynInfo :: Info
    } deriving (Generic,Eq,Show)
instance NFData DynamicName
instance Arbitrary DynamicName where
  arbitrary = DynamicName <$> genBareText <*> genBareText <*> arbitrary <*> arbitrary
instance ToJSON DynamicName
instance FromJSON DynamicName
instance HasInfo DynamicName where getInfo = _dynInfo
instance Pretty DynamicName where
  pretty DynamicName{..} = pretty _dynRefArg <> "." <> pretty _dynMember
instance AsString DynamicName where asString = renderCompactText
instance SizeOf DynamicName where
  sizeOf DynamicName{..} =
    sizeOf _dynMember + sizeOf _dynRefArg
    + sizeOf _dynInterfaces + sizeOf _dynInfo


-- | A named reference from source.
data Name
  = QName QualifiedName
  | Name BareName
  | DName DynamicName
  deriving (Generic, Show)

instance Arbitrary Name where
  -- assumes most names are qualified names
  arbitrary = frequency
    [ (4, QName <$> arbitrary)
    , (1, Name <$> arbitrary) ]

instance HasInfo Name where
  getInfo (QName q) = getInfo q
  getInfo (Name n) = getInfo n
  getInfo (DName d) = getInfo d

instance Pretty Name where
  pretty = \case
    QName q -> pretty q
    Name n -> pretty n
    DName d -> pretty d

instance SizeOf Name where
  sizeOf (QName qn) = (constructorCost 1) + sizeOf qn
  sizeOf (Name bn) = (constructorCost 1) + sizeOf bn
  sizeOf (DName dn) = (constructorCost 1) + sizeOf dn

instance AsString Name where asString = renderCompactText

instance ToJSON Name where
  toJSON = toJSON . renderCompactString

instance FromJSON Name where
  parseJSON = withText "Name" $ \t -> case parseName def t of
    Left s  -> fail s
    Right n -> return n

instance NFData Name

parseName :: Info -> Text -> Either String Name
parseName i = AP.parseOnly (nameParser i <* eof)


nameParser :: (TokenParsing m, Monad m) => Info -> m Name
nameParser i = (QName <$> qualifiedNameParser i <?> "qualifiedName") <|>
               (Name <$> bareNameParser <?> "bareName")
  where
    bareNameParser = BareName <$> ident style <*> pure i


instance Hashable Name where
  hashWithSalt s (Name (BareName t _)) =
    s `hashWithSalt` (0::Int) `hashWithSalt` t
  hashWithSalt s (QName (QualifiedName q n _)) =
    s `hashWithSalt` (1::Int) `hashWithSalt` q `hashWithSalt` n
  hashWithSalt s (DName DynamicName{..}) =
    s `hashWithSalt` (2::Int) `hashWithSalt` _dynMember
    `hashWithSalt` _dynRefArg `hashWithSalt` _dynInterfaces
instance Eq Name where
  (QName (QualifiedName a b _)) == (QName (QualifiedName c d _)) =
    (a,b) == (c,d)
  (Name (BareName a _)) == (Name (BareName b _)) =
    a == b
  (DName (DynamicName a b c _)) == (DName (DynamicName d e f _)) =
    (a,b,c) == (d,e,f)
  _ == _ = False
instance Ord Name where
  (QName (QualifiedName a b _)) `compare` (QName (QualifiedName c d _)) =
    (a,b) `compare` (c,d)
  (Name (BareName a _)) `compare` (Name (BareName b _)) =
    a `compare` b
  (DName (DynamicName a b c _)) `compare` (DName (DynamicName d e f _)) =
    (a,b,c) `compare` (d,e,f)
  Name {} `compare` QName {} = LT
  Name {} `compare` DName {} = LT
  QName {} `compare` DName {} = LT
  QName {} `compare` Name {} = GT
  DName {} `compare` Name {} = GT
  DName {} `compare` QName {} = GT


newtype NativeDefName = NativeDefName Text
    deriving (Eq,Ord,IsString,ToJSON,FromJSON,AsString,Show,NFData,Hashable)

instance Pretty NativeDefName where
  pretty (NativeDefName name) = pretty name



newtype TableName = TableName Text
    deriving (Eq,Ord,IsString,AsString,Hashable,Show,NFData,ToJSON,FromJSON)
instance Pretty TableName where pretty (TableName s) = pretty s

makeLenses ''ModuleName
