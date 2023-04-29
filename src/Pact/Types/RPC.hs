{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- |
-- Module      :  Pact.Types.RPC
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pact API RPC types.
--

module Pact.Types.RPC
  ( -- * Types
    PactRPC(..)
  , ExecMsg(..)
  , ContMsg(..)
  , contMsgJsonPairs
  ) where

import Control.Applicative
import Control.DeepSeq

-- Remove once migration is complete
import Data.Aeson (toEncoding)

import GHC.Generics

import Test.QuickCheck

import Pact.Types.Orphans ()
import Pact.Types.Runtime
import Pact.Types.SPV
import Pact.JSON.Legacy.Value

import Pact.JSON.Decode
import Pact.JSON.Value
import qualified Pact.JSON.Encode as J


data PactRPC c =
    Exec !(ExecMsg c) |
    Continuation !ContMsg
    deriving (Eq,Show,Generic,Functor,Foldable,Traversable)

instance NFData c => NFData (PactRPC c)
instance FromJSON c => FromJSON (PactRPC c) where
    parseJSON =
        withObject "RPC" $ \o ->
            (Exec <$> o .: "exec") <|> (Continuation <$> o .: "cont")
    {-# INLINE parseJSON #-}

pactRpcProperties :: ToJSON c => JsonProperties (PactRPC c)
pactRpcProperties (Exec p) = ["exec" .= p]
pactRpcProperties (Continuation p) = ["cont" .= p]

instance ToJSON c => ToJSON (PactRPC c) where
    toJSON = enableToJSON "Pact.Types.RPC.PactRPC" . object . pactRpcProperties
    toEncoding = pairs . mconcat . pactRpcProperties
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance J.Encode c => J.Encode (PactRPC c) where
  build (Exec p) = J.object ["exec" J..= p]
  build (Continuation p) = J.object ["cont" J..= p]
  {-# INLINE build #-}

instance Arbitrary c => Arbitrary (PactRPC c) where
  arbitrary = oneof [Exec <$> arbitrary, Continuation <$> arbitrary]

data ExecMsg c = ExecMsg
  { _pmCode :: !c
  , _pmData :: !LegacyValue
  } deriving (Eq,Generic,Show,Functor,Foldable,Traversable)

instance NFData c => NFData (ExecMsg c)
instance FromJSON c => FromJSON (ExecMsg c) where
  parseJSON =
      withObject "PactMsg" $ \o ->
          ExecMsg <$> o .: "code" <*> o .: "data"
  {-# INLINE parseJSON #-}

execMsgProperties :: ToJSON c => JsonProperties (ExecMsg c)
execMsgProperties o =
  [ "data" .= _pmData o
  , "code" .= _pmCode o
  ]
{-# INLINE execMsgProperties #-}

instance ToJSON c => ToJSON (ExecMsg c) where
  toJSON = enableToJSON "Pact.Types.RPC.ExecMsg" . object . execMsgProperties
  toEncoding = pairs . mconcat . execMsgProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode c => J.Encode (ExecMsg c) where
  build o = J.object
    [ "data" J..= _pmData o
    , "code" J..= _pmCode o
    ]
  {-# INLINE build #-}

instance Arbitrary c => Arbitrary (ExecMsg c) where
  arbitrary = ExecMsg <$> arbitrary <*> pure (toLegacyJson $ String "JSON VALUE")

data ContMsg = ContMsg
  { _cmPactId :: !PactId
  , _cmStep :: !Int
  , _cmRollback :: !Bool
  , _cmData :: !LegacyValue
  , _cmProof :: !(Maybe ContProof)
  } deriving (Eq,Show,Generic)

instance NFData ContMsg
instance FromJSON ContMsg where
  parseJSON =
      withObject "ContMsg" $ \o ->
          ContMsg <$> o .: "pactId" <*> o .: "step" <*> o .: "rollback" <*> o .: "data"
          <*> o .: "proof"
  {-# INLINE parseJSON #-}

contMsgProperties :: JsonProperties ContMsg
contMsgProperties o =
  [ "proof" .= _cmProof o
  , "data" .= _cmData o
  , "pactId" .= _cmPactId o
  , "rollback" .= _cmRollback o
  , "step" .= _cmStep o
  ]
{-# INLINE contMsgProperties #-}

instance ToJSON ContMsg where
  toJSON = enableToJSON "Pact.Types.RPC.ContMsg" . object . contMsgProperties
  toEncoding = pairs . mconcat . contMsgProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode ContMsg where
  build o = J.object
    [ "proof" J..= _cmProof o
    , "data" J..= _cmData o
    , "pactId" J..= _cmPactId o
    , "rollback" J..= _cmRollback o
    , "step" J..= J.Aeson (_cmStep o)
    ]
  {-# INLINE build #-}

instance Arbitrary ContMsg where
  arbitrary = ContMsg
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure (toLegacyJson $ String "JSON VALUE")
    <*> arbitrary

contMsgJsonPairs :: (Monoid a, KeyValue a) => ContMsg -> a
contMsgJsonPairs = mconcat . contMsgProperties
{-# INLINE contMsgJsonPairs #-}

