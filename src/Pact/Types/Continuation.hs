{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Pact.Types.Continuation
  ( -- * data
    PactStep(..)
  , PactContinuation(..)
  , PactExec(..)
  , Yield(..)
    -- * combinators
  , endorse
    -- * optics
  , peStepCount, peYield, peExecuted, pePactId, peStep, peContinuation
  , psStep, psRollback, psPactId, psResume
  , pcDef, pcArgs
  , yData, yTarget, yEndorsement
  ) where

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Lens

import Data.Aeson (ToJSON(..), FromJSON(..), encode)
import Data.ByteString.Lazy (toStrict)
import Data.Semigroup ((<>))

import Pact.Types.ChainMeta (ChainId)
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Term
import Pact.Types.Util (lensyToJSON, lensyParseJSON)

-- | The type of a set of yielded values of a pact step.
-- Endorsement hashes ('_yEndorsement') are a combination of
-- the following data:
--
-- 1. The hash of the data '_yData'
-- 2. The hash of the target chain '_yTarget'
-- 3. The executing pact id
-- 4. The hash of the defining module
--
data Yield = Yield
  { _yData :: !(Object Name)
    -- ^ Yield data from the pact continuation
  , _yTarget :: !ChainId
    -- ^ target chain on which to run continuation
  , _yEndorsement :: !Hash
  } deriving (Eq, Show, Generic)

instance NFData Yield
instance ToJSON Yield where toJSON = lensyToJSON 2
instance FromJSON Yield where parseJSON = lensyParseJSON 2

-- | Create a pact endorsement. Uses the 'PactHash' default
-- Blake2b_256 algorithm.
--
-- Endorsement hashes are a combination of
-- the following data:
--
-- 1. The hash of the data '_yData'
-- 2. The hash of the target chain '_yTarget'
-- 3. The executing pact id
-- 4. The hash of the defining module
--
endorse
  :: Hash
  -- ^ the hash of the containing module for a pact
  -> PactId
  -- ^ the executing pact id
  -> Object Name  -- ^ yield data
  -> ChainId
  -- ^ target chain id
  -> Hash
endorse (Hash mh) pid o tid =
  toUntypedHash . hash @('Blake2b_256) $!
    mh <> toStrict (encode pid <> encode o <> encode tid)

-- | Environment setup for pact execution, from ContMsg request.
--
data PactStep = PactStep {
      -- | intended step to execute
      _psStep :: !Int
      -- | rollback
    , _psRollback :: !Bool
      -- | pact id
    , _psPactId :: !PactId
      -- | resume value. Note that this is only set in Repl tests and in private use cases;
      -- in all other cases resume value comes out of PactExec.
    , _psResume :: !(Maybe (ObjectMap (Term Name)))
} deriving (Eq,Show)

instance Pretty PactStep where
  pretty = viaShow

-- | The type of pact continuations (i.e. defpact)
--
data PactContinuation = PactContinuation
  { _pcDef :: Name
  , _pcArgs :: [PactValue]
  } deriving (Eq, Show, Generic)

instance NFData PactContinuation
instance ToJSON PactContinuation where toJSON = lensyToJSON 3
instance FromJSON PactContinuation where parseJSON = lensyParseJSON 3

-- | Result of evaluation of a 'defpact'.
--
data PactExec = PactExec
  { -- | Count of steps in pact (discovered when code is executed)
    _peStepCount :: Int
    -- | Yield value if invoked
  , _peYield :: !(Maybe Yield)
    -- | Whether step was executed (in private cases, it can be skipped)
  , _peExecuted :: Bool
    -- | Step that was executed or skipped
  , _peStep :: Int
    -- | Pact id. On a new pact invocation, is copied from tx id.
  , _pePactId :: PactId
    -- | Strict (in arguments) application of pact, for future step invocations.
  , _peContinuation :: PactContinuation
  } deriving (Eq, Show, Generic)

instance NFData PactExec
instance ToJSON PactExec where toJSON = lensyToJSON 3
instance FromJSON PactExec where parseJSON = lensyParseJSON 3
instance Pretty PactExec where pretty = viaShow

makeLenses ''PactExec
makeLenses ''PactStep
makeLenses ''PactContinuation
makeLenses ''Yield
