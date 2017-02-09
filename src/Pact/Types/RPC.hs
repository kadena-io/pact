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
--  (Command(..)
--  ) where
  where

import Control.Applicative

import Data.Aeson as A
import Data.Text hiding (filter, null, all)


import GHC.Generics hiding (from)
import Prelude hiding (log,exp)

import Pact.Types.Runtime as Pact
import Pact.Types.Orphans ()

data PactRPC =
    Exec ExecMsg |
    Continuation ContMsg
    deriving (Eq,Show)
instance FromJSON PactRPC where
    parseJSON =
        withObject "RPC" $ \o ->
            (Exec <$> o .: "exec") <|> (Continuation <$> o .: "yield")
instance ToJSON PactRPC where
    toJSON (Exec p) = object ["exec" .= p]
    toJSON (Continuation p) = object ["yield" .= p]

class ToRPC a where
    toRPC :: a -> PactRPC
instance ToRPC ExecMsg where toRPC = Exec
instance ToRPC ContMsg where toRPC = Continuation

data ExecMsg = ExecMsg
  { _pmCode :: Text
  , _pmData :: Value
  } deriving (Eq,Generic,Show)
instance FromJSON ExecMsg where
    parseJSON =
        withObject "PactMsg" $ \o ->
            ExecMsg <$> o .: "code" <*> o .: "data"
instance ToJSON ExecMsg where
    toJSON (ExecMsg c d) = object [ "code" .= c, "data" .= d]

data ContMsg = ContMsg {
      _cmTxId :: TxId
    , _cmStep :: Int
    , _cmRollback :: Bool
    }
    deriving (Eq,Show)
instance FromJSON ContMsg where
    parseJSON =
        withObject "ContMsg" $ \o ->
            ContMsg <$> o .: "txid" <*> o .: "step" <*> o .: "rollback"
instance ToJSON ContMsg where
    toJSON (ContMsg t s r) = object [ "txid" .= t, "step" .= s, "rollback" .= r]
