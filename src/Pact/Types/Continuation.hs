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
  , Endorsement(..)
    -- * combinators
  , endorse
  , endorse'
    -- * optics
  , peStepCount, peYield, peExecuted, pePactId, peStep, peContinuation
  , psStep, psRollback, psPactId, psResume
  , pcDef, pcArgs
  , yData, yEndorsement
  , eTarget, ePactId, eHash
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



-- | Endorsement datatype contains all of the necessary
-- data to 'endorse' a yield object, detailed below
--
data Endorsement = Endorsement
  { _eTarget :: !ChainId
    -- ^ the target chain id for the endorsement
  , _ePactId :: PactId
    -- ^ the pact id of the current continuation
  , _eHash :: Hash
    -- ^ a hash of current containing module
  } deriving (Eq, Show, Generic)

instance NFData Endorsement
instance ToJSON Endorsement where toJSON = lensyToJSON 2
instance FromJSON Endorsement where parseJSON = lensyParseJSON 2

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
  { _yData :: !(ObjectMap PactValue)
    -- ^ Yield data from the pact continuation
  , _yEndorsement :: !(Maybe Endorsement)
    -- ^ The endorsement data, if it exists
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
  -> ObjectMap PactValue
  -- ^ yield data
  -> ChainId
  -- ^ target chain id
  -> Hash
endorse (Hash mh) pid o tid =
  toUntypedHash . hash @('Blake2b_256) $!
    mh <> toStrict (encode pid <> encode o <> encode tid)

-- | Given a 'Yield', if the endorsement value
-- is 'Nothing', return Nothing. If it contains a value,
-- return an optional hash of the endorsed values.
--
endorse' :: Yield -> Maybe Hash
endorse' (Yield _ Nothing) = Nothing
endorse' (Yield o e) = fmap (\(Endorsement tid pid h) -> endorse h pid o tid) e

-- | Environment setup for pact execution, from ContMsg request.
--
data PactStep = PactStep
  { _psStep :: !Int
    -- ^ intended step to execute
  , _psRollback :: !Bool
    -- ^ rollback
  , _psPactId :: !PactId
    -- ^ pact id
  , _psResume :: !(Maybe Yield)
    -- ^ resume value. Note that this is only set in Repl tests and in private use cases;
    -- in all other cases resume value comes out of PactExec.
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
  { _peStepCount :: Int
    -- ^ Count of steps in pact (discovered when code is executed)
  , _peYield :: !(Maybe Yield)
    -- ^ Yield value if invoked
  , _peExecuted :: Bool
    -- ^ Whether step was executed (in private cases, it can be skipped)
  , _peStep :: Int
    -- ^ Step that was executed or skipped
  , _pePactId :: PactId
    -- ^ Pact id. On a new pact invocation, is copied from tx id.
  , _peContinuation :: PactContinuation
    -- ^ Strict (in arguments) application of pact, for future step invocations.
  } deriving (Eq, Show, Generic)

instance NFData PactExec
instance ToJSON PactExec where toJSON = lensyToJSON 3
instance FromJSON PactExec where parseJSON = lensyParseJSON 3
instance Pretty PactExec where pretty = viaShow


makeLenses ''PactExec
makeLenses ''PactStep
makeLenses ''PactContinuation
makeLenses ''Yield
makeLenses ''Endorsement
