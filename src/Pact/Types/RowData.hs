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
    | RDList (Vector RowData)
    | RDObject (ObjectMap RowData)
    | RDGuard (Guard RowData)
    | RDModRef ModRef
    deriving (Eq,Show,Generic,Ord)
instance NFData RowDataValue

data RowDataVersion = RDV0 | RDV1
  deriving (Eq,Show,Generic,Ord,Enum,Bounded)
instance NFData RowDataVersion

data RowData = RowData RowDataVersion RowDataValue
  deriving (Eq,Show,Generic,Ord)
instance NFData RowData
instance Arbitrary RowData where
  arbitrary = pactValueToRowData <$> arbitraryBoundedEnum <*> arbitrary

pactValueToRowData :: RowDataVersion -> PactValue -> RowData
pactValueToRowData v pv = RowData v $ case pv of
  PLiteral l -> RDLiteral l
  PList l -> RDList $ recur l
  PObject o -> RDObject $ recur o
  PGuard g -> RDGuard $ recur g
  PModRef m -> RDModRef m
  where
    recur :: Functor f => f PactValue -> f RowData
    recur = fmap $ pactValueToRowData v

rowDataToPactValue :: RowData -> PactValue
rowDataToPactValue (RowData _ rdv) = case rdv of
  RDLiteral l -> PLiteral l
  RDList l -> PList $ recur l
  RDObject o -> PObject $ recur o
  RDGuard g -> PGuard $ recur g
  RDModRef m -> PModRef m
  where
    recur :: Functor f => f RowData -> f PactValue
    recur = fmap $ rowDataToPactValue

instance Pretty RowData where
  pretty = pretty . rowDataToPactValue


instance ToJSON RowData where
  toJSON rd@(RowData RDV0 _) = toJSON $ rowDataToPactValue rd
  toJSON (RowData RDV1 v) = case v of
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

instance FromJSON RowData where
  parseJSON v =
    (RowData RDV1 <$> parseV1 v) <|>
    (pactValueToRowData RDV0 <$> parseJSON v)
    where
      parseV1 v1 =
        (RDLiteral <$> parseJSON v1) <|>
        (RDList <$> parseJSON v1) <|>
        parseTagged v1
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
