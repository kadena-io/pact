{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Pact.Types.RowData
-- Copyright   :  (C) 2021 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Versioned persistence for 'PactValue'.
--
module Pact.Types.RowData
  ( RowData(..)
  , RowDataVersion(..)
  , RowDataValue(..)
  , pactValueToRowData
  , rowDataToPactValue
  ) where

import Control.Applicative
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Aeson.Encoding
import Data.Default
import Data.Maybe(fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Test.QuickCheck

import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.PactValue.Arbitrary ()
import Pact.Types.Pretty
import Pact.Types.Term
import Pact.Types.Util

import qualified Pact.JSON.Encode as J

-- -------------------------------------------------------------------------- --
-- RowDataValue

data RowDataValue
    = RDLiteral Literal
    | RDList (Vector RowDataValue)
    | RDObject (ObjectMap RowDataValue)
    | RDGuard (Guard RowDataValue)
    | RDModRef ModRef
    deriving (Eq,Show,Generic,Ord)
instance NFData RowDataValue
instance Arbitrary RowDataValue where
  arbitrary = pactValueToRowData <$> arbitrary

tag :: ToJSON v => KeyValue kv => Text -> v -> [kv]
tag t rv =
  [ "$t" .= t
  , "$v" .= rv
  ]
{-# INLINE tag #-}

instance ToJSON RowDataValue where
  toJSON = enableToJSON "Pact.Types.RowData.RowDataValue" . \case
    RDLiteral l -> toJSON l
    RDList l -> toJSON l
    RDObject o -> object $ tag "o" o
    RDGuard g -> object $ tag "g" g
    RDModRef (ModRef refName refSpec _) -> object $ tag "m" $ object
      [ "refSpec" .= refSpec
      , "refName" .= refName
      ]
  {-# INLINE toJSON #-}

  toEncoding rdv = case rdv of
    RDLiteral l -> toEncoding l
    RDList l -> toEncoding l
    RDObject o -> pairs . mconcat $ tag "o" o
    RDGuard g -> pairs . mconcat $ tag "g" g
    RDModRef (ModRef refName refSpec _) -> pairs . mconcat $
      [ "$t" .= ("m" :: Text)
      , pair "$v" $ pairs $ mconcat
          [ "refSpec" .= refSpec
          , "refName" .= refName
          ]
      ]
  {-# INLINE toEncoding #-}

instance FromJSON RowDataValue where
  parseJSON v1 =
    (RDLiteral <$> parseJSON v1) <|>
    (RDList <$> parseJSON v1) <|>
    parseTagged v1
    where
      parseTagged = withObject "tagged RowData" $ \o -> do
        (t :: Text) <- o .: "$t"
        val <- o .: "$v"
        case t of
          "o" -> RDObject <$> parseJSON val
          "g" -> RDGuard <$> parseJSON val
          "m" -> RDModRef <$> parseMR val
          _ -> fail "tagged RowData"
      parseMR = withObject "tagged ModRef" $ \o -> ModRef
          <$> o .: "refName"
          <*> o .: "refSpec"
          <*> pure def

instance J.Encode RowDataValue where
  build (RDLiteral l) = J.build l
  build (RDList l) = J.build $ J.array l
  build (RDObject o) = tagged "o" o
  build (RDGuard g) = tagged "g" g
  build (RDModRef (ModRef refName refSpec _)) = J.object
    [ "$t" J..= J.text "m"
    , "$v" J..= J.object
      [ "refSpec" J..= (J.array <$> refSpec)
      , "refName" J..= refName
      ]
    ]
  {-# INLINE build #-}

tagged :: J.Encode v => Text -> v -> J.Builder
tagged t rv = J.object
  [ "$t" J..= t
  , "$v" J..= rv
  ]
{-# INLINE tagged #-}

-- -------------------------------------------------------------------------- --
-- RowDataVersion

data RowDataVersion = RDV0 | RDV1
  deriving (Eq,Show,Generic,Ord,Enum,Bounded)

instance NFData RowDataVersion

instance Arbitrary RowDataVersion where
  arbitrary = elements [RDV0, RDV1]

instance ToJSON RowDataVersion where
  toJSON = toJSON . fromEnum
  toEncoding = toEncoding . fromEnum
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON RowDataVersion where
  parseJSON = withScientific "RowDataVersion" $ \case
    0 -> pure RDV0
    1 -> pure RDV1
    _ -> fail "RowDataVersion"

instance J.Encode RowDataVersion where
  build = J.build . J.Aeson . fromEnum
  {-# INLINE build #-}

-- -------------------------------------------------------------------------- --
-- RowData

data RowData = RowData
    { _rdVersion :: RowDataVersion
    , _rdData :: ObjectMap RowDataValue
    }
  deriving (Eq,Show,Generic,Ord)
instance NFData RowData
instance Pretty RowData where pretty (RowData _ m) = pretty m

instance Arbitrary RowData where
  arbitrary = RowData <$> arbitrary <*> arbitrary

pactValueToRowData :: PactValue -> RowDataValue
pactValueToRowData pv = case pv of
  PLiteral l -> RDLiteral l
  PList l -> RDList $ recur l
  PObject o -> RDObject $ recur o
  PGuard g -> RDGuard $ recur g
  PModRef m -> RDModRef m
  where
    recur :: Functor f => f PactValue -> f RowDataValue
    recur = fmap pactValueToRowData

rowDataToPactValue :: RowDataValue -> PactValue
rowDataToPactValue rdv = case rdv of
  RDLiteral l -> PLiteral l
  RDList l -> PList $ recur l
  RDObject o -> PObject $ recur o
  RDGuard g -> PGuard $ recur g
  RDModRef m -> PModRef m
  where
    recur :: Functor f => f RowDataValue -> f PactValue
    recur = fmap rowDataToPactValue

instance Pretty RowDataValue where
  pretty = pretty . rowDataToPactValue

rowDataProperties :: JsonProperties RowData
rowDataProperties o =
  [ "$d" .= _rdData o
  , "$v" .= _rdVersion o
  ]
{-# INLINE rowDataProperties #-}

instance ToJSON RowData where
  toJSON (RowData RDV0 m) = toJSON $ fmap rowDataToPactValue m
  toJSON r = object $ rowDataProperties r

  toEncoding (RowData RDV0 m) = toEncoding $ fmap rowDataToPactValue m
  toEncoding r = pairs $ mconcat $ rowDataProperties r

  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode RowData where
  build (RowData RDV0 m) = J.build $ fmap rowDataToPactValue m
  build o = J.object
    [ "$d" J..= _rdData o
    , "$v" J..= _rdVersion o
    ]
  {-# INLINE build #-}

instance FromJSON RowData where
  parseJSON v =
    parseVersioned v <|>
    -- note: Parsing into `OldPactValue` here defaults to the code used in
    -- the old FromJSON instance for PactValue, prior to the fix of moving
    -- the `PModRef` parsing before PObject
    RowData RDV0 . fmap oldPactValueToRowData <$> parseJSON v
    where
      oldPactValueToRowData = \case
        OldPLiteral l -> RDLiteral l
        OldPList l -> RDList $ recur l
        OldPObject o -> RDObject $ recur o
        OldPGuard g -> RDGuard $ recur g
        OldPModRef m -> RDModRef m
      recur :: Functor f => f OldPactValue -> f RowDataValue
      recur = fmap oldPactValueToRowData
      parseVersioned = withObject "RowData" $ \o -> RowData
          <$> o .: "$v"
          <*> o .: "$d"

-- -------------------------------------------------------------------------- --
-- OldPactValue

data OldPactValue
  = OldPLiteral Literal
  | OldPList (Vector OldPactValue)
  | OldPObject (ObjectMap OldPactValue)
  | OldPGuard (Guard OldPactValue)
  | OldPModRef ModRef

-- Needed for parsing guard
instance ToJSON OldPactValue where
  toJSON = enableToJSON "Pact.Types.RowData.OldPactValue" . \case
    OldPLiteral l -> toJSON l
    OldPObject o -> toJSON o
    OldPList v -> toJSON v
    OldPGuard x -> toJSON x
    OldPModRef m -> Data.Aeson.Object $ modRefProperties_ m
      -- this uses a non-standard alternative JSON encoding for 'ModRef'

  toEncoding = \case
    OldPLiteral l -> toEncoding l
    OldPObject o -> toEncoding o
    OldPList v -> toEncoding v
    OldPGuard x -> toEncoding x
    OldPModRef m -> pairs $ modRefProperties_ m
      -- this uses a non-standard alternative JSON encoding for 'ModRef'

  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode OldPactValue where
  build (OldPLiteral l) = J.build l
  build (OldPObject o) = J.build o
  build (OldPList v) = J.array v
  build (OldPGuard x) = J.build x
  build (OldPModRef m) = J.object $ modRefKeyValues_ m
      -- this uses a non-standard alternative JSON encoding for 'ModRef'
  {-# INLINE build #-}

instance FromJSON OldPactValue where
  parseJSON v =
    (OldPLiteral <$> parseJSON v) <|>
    (OldPList <$> parseJSON v) <|>
    (OldPGuard <$> parseJSON v) <|>
    (OldPObject <$> parseJSON v) <|>
    (OldPModRef <$> (parseNoInfo v <|> parseJSON v))
    where
      parseNoInfo = withObject "ModRef" $ \o -> ModRef
        <$> o .: "refName"
        <*> o .: "refSpec"
        <*> (fromMaybe def <$> o .:? "refInfo")

