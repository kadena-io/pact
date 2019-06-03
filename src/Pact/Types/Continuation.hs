{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Module      :  Pact.Types.Continuation
-- Copyright   :  (C) 2016 Stuart Popejoy, Emily Pillmore
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, Emily Pillmore <emily@kadena.io>
--
-- Pact Continuation data
--
module Pact.Types.Continuation
  ( -- * data
    PactStep(..)
  , PactContinuation(..)
  , PactExec(..)
  , Yield(..)
  , Provenance(..)
    -- * optics
  , peStepCount, peYield, peExecuted, pePactId, peStep, peContinuation
  , psStep, psRollback, psPactId, psResume
  , pcDef, pcArgs
  , yData, yProvenance
  , pTargetChainId, pModuleHash
  ) where

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Lens

import Data.Aeson (ToJSON(..), FromJSON(..))

import Pact.Types.ChainId (ChainId)
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.Term
import Pact.Types.Util (lensyToJSON, lensyParseJSON)


-- | Provenance datatype contains all of the necessary
-- data to 'endorse' a yield object.
--
data Provenance = Provenance
  { _pTargetChainId :: !ChainId
    -- ^ the target chain id for the endorsement
  , _pModuleHash :: ModuleHash
    -- ^ a hash of current containing module
  } deriving (Eq, Show, Generic)

instance NFData Provenance
instance ToJSON Provenance where toJSON = lensyToJSON 2
instance FromJSON Provenance where parseJSON = lensyParseJSON 2

-- | The type of a set of yielded values of a pact step.
--
data Yield = Yield
  { _yData :: !(ObjectMap PactValue)
    -- ^ Yield data from the pact continuation
  , _yProvenance :: !(Maybe Provenance)
    -- ^ Provenance data
  } deriving (Eq, Show, Generic)

instance NFData Yield
instance ToJSON Yield where toJSON = lensyToJSON 2
instance FromJSON Yield where parseJSON = lensyParseJSON 2

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
makeLenses ''Provenance
