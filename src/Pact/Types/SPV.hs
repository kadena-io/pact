{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Pact.Types.SPV
-- Copyright   :  (C) 2019 Stuart Popejoy, Emily Pillmore
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>,
--                Emily Pillmore <emily@kadena.io>
--
-- SPV Support data and types
--
module Pact.Types.SPV
  ( -- * Types
    ContProof(..)
  , SPVSupport(..)
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
import Data.Default (def)
import Data.Text
import Data.Text.Encoding

import GHC.Generics hiding (to)

import Test.QuickCheck

import Pact.Types.Continuation (PactExec(PactExec), PactContinuation(PactContinuation))
import Pact.Types.Pretty (Pretty(..), prettyString)
import Pact.Types.Term (Object, Name(Name), PactId(PactId), BareName(BareName))

import qualified Pact.JSON.Encode as J

newtype ContProof = ContProof { _contProof :: ByteString }
  deriving (Eq, Ord, Show, Generic)

instance Wrapped ContProof

instance NFData ContProof

instance J.Encode ContProof where
  build (ContProof bs) = J.build $ decodeUtf8 bs
  {-# INLINE build #-}

instance FromJSON ContProof where
  parseJSON = withText "ByteString" (return . ContProof . encodeUtf8)
instance Pretty ContProof where
  pretty = prettyString . show

instance Arbitrary ContProof where
  arbitrary = ContProof . encodeUtf8 <$> arbitrary

-- | Backend for SPV support
data SPVSupport = SPVSupport
  { _spvSupport :: !(Text -> (Object Name) -> IO (Either Text (Object Name)))
    -- ^ Attempt to verify an SPV proof of a given type,
    -- given a payload object. On success, returns the
    -- specific data represented by the proof.
  , _spvVerifyContinuation :: !(ContProof -> IO (Either Text PactExec))
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
