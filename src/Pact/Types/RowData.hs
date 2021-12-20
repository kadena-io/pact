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
import Data.Default
import Data.Maybe(fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics
import Test.QuickCheck

import Pact.Types.Exp
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Term

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

instance ToJSON RowDataValue where
  toJSON rdv = case rdv of
    RDLiteral l -> toJSON l
    RDList l -> toJSON l
    RDObject o -> tag "o" o
    RDGuard g -> tag "g" g
    RDModRef (ModRef refName refSpec _) -> tag "m" $ object
        [ "refName" .= refName
        , "refSpec" .= refSpec
        ]
    where
      tag :: ToJSON t => Text -> t -> Value
      tag t rv = object [ "$t" .= t, "$v" .= rv ]

instance FromJSON RowDataValue where
  parseJSON v1 =
    (RDLiteral <$> parseJSON v1) <|>
    (RDList <$> parseJSON v1) <|>
    parseTagged v1
    where
      parseTagged = withObject "tagged RowData" $ \o -> do
        (tag :: Text) <- o .: "$t"
        val <- o .: "$v"
        case tag of
          "o" -> RDObject <$> parseJSON val
          "g" -> RDGuard <$> parseJSON val
          "m" -> RDModRef <$> parseMR val
          _ -> fail "tagged RowData"
      parseMR = withObject "tagged ModRef" $ \o -> ModRef
          <$> o .: "refName"
          <*> o .: "refSpec"
          <*> pure def


data RowDataVersion = RDV0 | RDV1
  deriving (Eq,Show,Generic,Ord,Enum,Bounded)
instance NFData RowDataVersion
instance ToJSON RowDataVersion where
  toJSON = toJSON . fromEnum
instance FromJSON RowDataVersion where
  parseJSON = withScientific "RowDataVersion" $ \case
    0 -> pure RDV0
    1 -> pure RDV1
    _ -> fail "RowDataVersion"

data RowData = RowData
    { _rdVersion :: RowDataVersion
    , _rdData :: ObjectMap RowDataValue
    }
  deriving (Eq,Show,Generic,Ord)
instance NFData RowData
instance Pretty RowData where pretty (RowData _ m) = pretty m

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


instance ToJSON RowData where
  toJSON (RowData RDV0 m) = toJSON $ fmap rowDataToPactValue m
  toJSON (RowData v m) = object
      [ "$v" .= v, "$d" .= m ]

newtype OldPactValue =
  OldPactValue { unP :: PactValue }

instance FromJSON OldPactValue where
  parseJSON v = fmap OldPactValue $
    (PLiteral <$> parseJSON v) <|>
    (PList <$> parseJSON v) <|>
    (PGuard <$> parseJSON v) <|>
    (PObject <$> parseJSON v) <|>
    (PModRef <$> (parseNoInfo v <|> parseJSON v))
    where
      parseNoInfo = withObject "ModRef" $ \o -> ModRef
        <$> o .: "refName"
        <*> o .: "refSpec"
        <*> (fromMaybe def <$> o .:? "refInfo")

instance FromJSON RowData where
  parseJSON v =
    parseVersioned v <|>
    -- note: Parsing into `OldPactValue` here defaults to the code used in
    -- the old FromJSON instance for PactValue, prior to the fix of moving
    -- the `PModRef` parsing before PObject
    RowData RDV0 . fmap (pactValueToRowData . unP) <$> parseJSON v
    where
      parseVersioned = withObject "RowData" $ \o -> RowData
          <$> o .: "$v"
          <*> o .: "$d"
