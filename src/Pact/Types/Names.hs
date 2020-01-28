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
  , ModuleName(..), mnName, mnNamespace
  , Name(..), parseName
  , QualifiedName(..), parseQualifiedName
  , BareName(..)
  ) where


import Control.Applicative
import Control.DeepSeq
import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import qualified Data.Attoparsec.Text as AP
import Data.Default
import Data.Either
import Data.Function
import Data.Hashable
import Data.Maybe
import Data.String
import Data.Text (Text,pack)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude
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

-- | A named reference from source.
data Name
  = QName QualifiedName
  | Name BareName
  deriving (Generic, Show)

instance Arbitrary Name where
  -- assumes most names are qualified names
  arbitrary = frequency
    [ (4, QName <$> arbitrary)
    , (1, Name <$> arbitrary) ]

instance HasInfo Name where
  getInfo (QName q) = getInfo q
  getInfo (Name n) = getInfo n

instance Pretty Name where
  pretty = \case
    QName q -> pretty q
    Name n -> pretty n

instance SizeOf Name where
  sizeOf (QName qn) = (constructorCost 1) + sizeOf qn
  sizeOf (Name bn) = (constructorCost 1) + sizeOf bn

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
  hashWithSalt s (Name (BareName t _)) = s `hashWithSalt` (0::Int) `hashWithSalt` t
  hashWithSalt s (QName (QualifiedName q n _)) = s `hashWithSalt` (1::Int) `hashWithSalt` q `hashWithSalt` n
instance Eq Name where
  (QName (QualifiedName a b _)) == (QName (QualifiedName c d _)) = (a,b) == (c,d)
  (Name (BareName a _)) == (Name (BareName b _)) = a == b
  _ == _ = False
instance Ord Name where
  (QName (QualifiedName a b _)) `compare` (QName (QualifiedName c d _)) = (a,b) `compare` (c,d)
  (Name (BareName a _)) `compare` (Name (BareName b _)) = a `compare` b
  Name {} `compare` QName {} = LT
  QName {} `compare` Name {} = GT


newtype NativeDefName = NativeDefName Text
    deriving (Eq,Ord,IsString,ToJSON,FromJSON,AsString,Show,NFData,Hashable)

instance Pretty NativeDefName where
  pretty (NativeDefName name) = pretty name



newtype TableName = TableName Text
    deriving (Eq,Ord,IsString,AsString,Hashable,Show,NFData,ToJSON,FromJSON)
instance Pretty TableName where pretty (TableName s) = pretty s

makeLenses ''ModuleName
