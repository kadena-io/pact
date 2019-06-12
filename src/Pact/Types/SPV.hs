{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Pact.Types.Runtime
-- Copyright   :  (C) 2019 Stuart Popejoy, Emily Pillmore
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>,
--                Emily Pillmore <emily@kadena.io>
--
-- SPV Support data and types
--
module Pact.Types.SPV
  ( -- Types
    ContProof(..)
  , SPVSupport
  , SPV
  , ContSPV
    -- * Support
  , noSPVSupport
    -- * Optics
  , spvSupport
  , spvVerifyContinuation
  ) where

import Control.DeepSeq (NFData)
import Control.Lens

import Data.Aeson hiding (Object)
import Data.ByteString
import Data.Text
import Data.Text.Encoding

import GHC.Generics hiding (to)

import Pact.Types.Continuation (PactExec)
import Pact.Types.Term (Object, Name)


type SPV = Text -> Object Name -> IO (Either Text (Object Name))
type ContSPV = ContProof -> IO (Either Text PactExec)

newtype ContProof = ContProof { _contProof :: ByteString }
  deriving (Eq,Show,Generic)

instance Wrapped ContProof

instance NFData ContProof
instance ToJSON ContProof where
  toJSON (ContProof bs) = String (decodeUtf8 bs)
instance FromJSON ContProof where
  parseJSON = withText "ByteString" (return . ContProof . encodeUtf8)

-- | Backend for SPV support
data SPVSupport = SPVSupport
  { _spvSupport :: Text -> Object Name -> IO (Either Text (Object Name))
    -- ^ Attempt to verify an SPV proof of a given type,
    -- given a payload object. On success, returns the
    -- specific data represented by the proof.
  , _spvVerifyContinuation :: ContProof -> IO (Either Text PactExec)
    -- ^ Attempt to verify an SPV proof of a continuation given
    -- a continuation payload object bytestring. On success, returns
    -- the 'PactExec' associated with the proof.
  }
makeLenses ''SPVSupport

noSPVSupport :: SPVSupport
noSPVSupport = SPVSupport spv vcon
  where
    spv = \_ _ -> return $ Left "SPV verification not supported"
    vcon = \_ -> return $ Left "Cross-chain continuations not supported"
