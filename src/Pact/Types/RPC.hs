{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
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
  ) where

import Control.Applicative
import Control.DeepSeq

import Data.Aeson as A

import GHC.Generics

import Pact.Types.Orphans ()
import Pact.Types.Runtime
import Pact.Types.SPV


data PactRPC c =
    Exec (ExecMsg c) |
    Continuation ContMsg
    deriving (Eq,Show,Generic,Functor,Foldable,Traversable)

instance NFData c => NFData (PactRPC c)
instance FromJSON c => FromJSON (PactRPC c) where
    parseJSON =
        withObject "RPC" $ \o ->
            (Exec <$> o .: "exec") <|> (Continuation <$> o .: "cont")
    {-# INLINE parseJSON #-}

instance ToJSON c => ToJSON (PactRPC c) where
    toJSON (Exec p) = object ["exec" .= p]
    toJSON (Continuation p) = object ["cont" .= p]



data ExecMsg c = ExecMsg
  { _pmCode :: c
  , _pmData :: Value
  } deriving (Eq,Generic,Show,Functor,Foldable,Traversable)

instance NFData c => NFData (ExecMsg c)
instance FromJSON c => FromJSON (ExecMsg c) where
    parseJSON =
        withObject "PactMsg" $ \o ->
            ExecMsg <$> o .: "code" <*> o .: "data"
    {-# INLINE parseJSON #-}

instance ToJSON c => ToJSON (ExecMsg c) where
    toJSON (ExecMsg c d) = object [ "code" .= c, "data" .= d]

data ContMsg = ContMsg
  { _cmPactId :: !PactId
  , _cmStep :: !Int
  , _cmRollback :: !Bool
  , _cmData :: !Value
  , _cmProof :: !(Maybe ContProof)
  } deriving (Eq,Show,Generic)

instance NFData ContMsg
instance FromJSON ContMsg where
    parseJSON =
        withObject "ContMsg" $ \o ->
            ContMsg <$> o .: "pactId" <*> o .: "step" <*> o .: "rollback" <*> o .: "data"
            <*> o .: "proof"
    {-# INLINE parseJSON #-}

instance ToJSON ContMsg where
    toJSON ContMsg{..} = object
      [ "pactId" .= _cmPactId, "step" .= _cmStep, "rollback" .= _cmRollback, "data" .= _cmData, "proof" .= _cmProof]
