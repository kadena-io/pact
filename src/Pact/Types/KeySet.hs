{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Pact.Types.Keyset
-- Copyright   :  (C) 2022 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- A Pact Keyset encapsulates the notion of a 'PublicKey'
-- into a predicate that matches the image of the public key
-- against some set in the environment representing public keys
-- used to sign the currently operational transaction.
--
-- This is an opinionated approach designed to guarantee that
-- whenever a single key is used as a guard,
-- a multi-sig set can also be used.
--
-- This module also includes 'KeyFormat' and related functions
-- for validating keyset key formats.
--

module Pact.Types.KeySet
  ( PublicKey(..)
  , KeySet(..)
  , KeySetName(..)
  , mkKeySet
  , KeyFormat
  , ed25519Hex
  , isHexDigitLower
  , keyFormats
  , validateKeyFormat
  , enforceKeyFormats
  , keysetNameParser
  , qualifiedKeysetNameParser
  , parseAnyKeysetName
  , parseQualifiedKeySetName
  , ksKeys
  , ksPredFun
  ) where

import Control.Applicative ( Alternative((<|>)) )
import Control.DeepSeq
import Control.Monad
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Attoparsec.Text (parseOnly)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import Data.Char
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Serialize (Serialize)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics
import Test.QuickCheck

import Pact.Types.Names
import Pact.Types.Pretty hiding (dot)
import Pact.Types.SizeOf
import Pact.Types.Util
import Pact.Types.Parser (style)

import Text.Parser.Combinators (eof)
import Text.Parser.Token

-- -------------------------------------------------------------------------- --
-- PublicKey

newtype PublicKey = PublicKey { _pubKey :: ByteString }
  deriving (Eq,Ord,Generic,IsString,AsString,Show,SizeOf)

instance Arbitrary PublicKey where
  arbitrary = PublicKey . encodeUtf8 . T.pack <$> vectorOf 64 genValidPublicKeyChar
    where genValidPublicKeyChar = suchThat arbitraryASCIIChar isAlphaNum
instance Serialize PublicKey
instance NFData PublicKey
instance FromJSON PublicKey where
  parseJSON = withText "PublicKey" (return . PublicKey . encodeUtf8)
instance ToJSON PublicKey where
  toJSON = toJSON . decodeUtf8 . _pubKey

instance Pretty PublicKey where
  pretty (PublicKey s) = prettyString (BSU.toString s)

-- -------------------------------------------------------------------------- --
-- KeySet

-- | KeySet pairs keys with a predicate function name.
data KeySet = KeySet
  { _ksKeys :: !(Set PublicKey)
  , _ksPredFun :: !Name
  } deriving (Eq,Generic,Show,Ord)
makeLenses ''KeySet

instance NFData KeySet

instance Pretty KeySet where
  pretty (KeySet ks f) = "KeySet" <+> commaBraces
    [ "keys: " <> prettyList (toList ks)
    , "pred: " <> pretty f
    ]

instance SizeOf KeySet where
  sizeOf (KeySet pkArr ksPred) =
    (constructorCost 2) + (sizeOf pkArr) + (sizeOf ksPred)

instance Arbitrary KeySet where
  arbitrary = do
    pks <- listOf1 arbitrary
    name <- frequency
      [ (3, pure "keys-all")
      , (2, pure "keys-any")
      , (1, pure "keys-2")
      , (1, genBareText)
      ]
    pure $ mkKeySet pks name

-- | allow `{ "keys": [...], "pred": "..." }`, `{ "keys": [...] }`, and just `[...]`,
-- | the latter cases defaulting to "keys-all"
instance FromJSON KeySet where
    parseJSON v = withObject "KeySet" keyListPred v <|> keyListOnly
      where
        defPred = Name (BareName "keys-all" def)

        keyListPred o = KeySet
          <$> o .: "keys"
          <*> (fromMaybe defPred <$> o .:? "pred")

        keyListOnly = KeySet
          <$> parseJSON v
          <*> pure defPred

instance ToJSON KeySet where
    toJSON (KeySet k f) = object
      [ "keys" .= k
      , "pred" .= f
      ]

-- -------------------------------------------------------------------------- --
-- KeySetName

data KeySetName
  = KeySetName
  { _ksnName :: Text
  , _ksnNamespace :: Maybe NamespaceName
  } deriving (Eq, Ord, Show, Generic)

instance IsString KeySetName where
  fromString ksn = case parseAnyKeysetName (T.pack ksn) of
    Left e -> error e
    Right ks -> ks

instance NFData KeySetName

instance SizeOf KeySetName where
  sizeOf = sizeOf . asString

instance FromJSON KeySetName where
  parseJSON v =
    newKs v <|> oldKs v
    where
    oldKs = withText "KeySetName" (pure . (`KeySetName` Nothing))
    newKs =
      withObject "KeySetName" $ \o -> KeySetName
        <$> o .: "name"
        <*> (fromMaybe Nothing <$> o .:? "ns")

instance ToJSON KeySetName where
  toJSON (KeySetName k n) = case n of
    Nothing -> toJSON k
    Just ns -> object [ "ksn" .= k , "ns" .= ns ]

instance AsString KeySetName where
  asString (KeySetName n mns) = case mns of
    Nothing -> n
    Just (NamespaceName ns) -> ns <> "." <> n

instance Arbitrary KeySetName where
  arbitrary = KeySetName
    <$> genBareText
    <*> (Just . NamespaceName <$> genBareText)

instance Pretty KeySetName where
  pretty ksn = "'" <> pretty (asString ksn)

keysetNameParser
  :: TokenParsing m
  => Monad m
  => m KeySetName
keysetNameParser =
  qualifiedKeysetNameParser <|> woNs
  where
    woNs = (`KeySetName` Nothing) <$> (ident style <* eof)

qualifiedKeysetNameParser
  :: TokenParsing m
  => Monad m
  => m KeySetName
qualifiedKeysetNameParser = do
  ns <- NamespaceName <$> ident style
  kn <- dot *> ident style <* eof
  pure $ KeySetName kn (Just ns)

parseAnyKeysetName
  :: Text
  -> Either String KeySetName
parseAnyKeysetName
  = parseOnly keysetNameParser

parseQualifiedKeySetName
  :: Text
  -> Either String KeySetName
parseQualifiedKeySetName
  = parseOnly qualifiedKeysetNameParser

-- | Smart constructor for a simple list and barename predicate.
mkKeySet :: [PublicKey] -> Text -> KeySet
mkKeySet pks p = KeySet
  (S.fromList pks)
  (Name $ BareName p def)

-- | A predicate for public key format validation.
type KeyFormat = PublicKey -> Bool

-- | Current "Kadena" ED-25519 key format: 64-length hex.
ed25519Hex :: KeyFormat
ed25519Hex (PublicKey k) = BSC.length k == 64 && BSC.all isHexDigitLower k

-- | Lower-case hex numbers.
isHexDigitLower :: Char -> Bool
isHexDigitLower c =
  -- adapted from GHC.Unicode#isHexDigit
  isDigit c || (fromIntegral (ord c - ord 'a')::Word) <= 5

-- | Supported key formats.
keyFormats :: [KeyFormat]
keyFormats = [ed25519Hex]

-- | Validate 'PublicKey' against 'keyFormats'.
validateKeyFormat :: PublicKey -> Bool
validateKeyFormat k = any ($ k) keyFormats

-- | Enforce valid 'KeySet' keys, evaluating error action on failure.
enforceKeyFormats :: Monad m => (PublicKey -> m ()) -> KeySet -> m ()
enforceKeyFormats err (KeySet ks _p) = traverse_ go ks
  where
    go k = unless (validateKeyFormat k) $ err k
