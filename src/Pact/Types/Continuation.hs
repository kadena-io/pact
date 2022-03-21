{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Pact.Types.Continuation
-- Copyright   :  (C) 2019 Stuart Popejoy, Emily Pillmore
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, Emily Pillmore <emily@kadena.io>
--
-- Pact Continuation data
--
module Pact.Types.Continuation
  ( -- * Types
    PactStep(..)
  , PactContinuation(..)
  , PactExec(..)
  , Yield(..)
  , Provenance(..)
    -- * Optics
  , peStepCount, peYield, peExecuted, pePactId
  , peStep, peContinuation, peStepHasRollback
  , psStep, psRollback, psPactId, psResume
  , pcDef, pcArgs
  , yData, yProvenance, ySourceChain
  , pTargetChainId, pModuleHash
  ) where

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Lens hiding ((.=))

import Data.Aeson

import Test.QuickCheck

import Pact.Types.ChainId (ChainId)
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.SizeOf
import Pact.Types.Term
import Pact.Types.Util (lensyToJSON, lensyParseJSON, JsonProperties, JsonMProperties, enableToJSON, (.?=))


-- | Provenance datatype contains all of the necessary
-- data to 'endorse' a yield object.
--
data Provenance = Provenance
  { _pTargetChainId :: !ChainId
    -- ^ the target chain id for the endorsement
  , _pModuleHash :: ModuleHash
    -- ^ a hash of current containing module
  } deriving (Eq, Show, Generic)

instance Arbitrary Provenance where
  arbitrary = Provenance <$> arbitrary <*> arbitrary

instance Pretty Provenance where
  pretty (Provenance c h) = "(chain = \"" <> pretty c <> "\" hash=\"" <> pretty h <> "\")"

instance SizeOf Provenance where
  sizeOf (Provenance chainId modHash) =
    (constructorCost 2) + (sizeOf chainId) + (sizeOf modHash)

instance NFData Provenance

provenanceProperties :: JsonProperties Provenance
provenanceProperties o =
  [ "targetChainId" .= _pTargetChainId o
  , "moduleHash" .= _pModuleHash o
  ]
{-# INLINE provenanceProperties #-}

instance ToJSON Provenance where
  toJSON = enableToJSON "Pact.Types.Continuation.Provenance" . lensyToJSON 2
  toEncoding = pairs . mconcat . provenanceProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON Provenance where parseJSON = lensyParseJSON 2

-- | The type of a set of yielded values of a pact step.
--
data Yield = Yield
  { _yData :: !(ObjectMap PactValue)
    -- ^ Yield data from the pact continuation
  , _yProvenance :: !(Maybe Provenance)
    -- ^ Provenance data
  , _ySourceChain :: !(Maybe ChainId)
  } deriving (Eq, Show, Generic)

instance Arbitrary Yield where
  arbitrary = Yield <$> (genPactValueObjectMap RecurseTwice) <*> frequency
    [ (1, pure Nothing)
    , (4, Just <$> arbitrary) ] <*> pure Nothing

instance NFData Yield

yieldProperties :: JsonMProperties Yield
yieldProperties o = mconcat
  [ "data" .= _yData o
  , "source" .?= _ySourceChain o
  , "provenance" .= _yProvenance o
  ]
{-# INLINE yieldProperties #-}

instance ToJSON Yield where
  toJSON = enableToJSON "Pact.Types.Continuation.Yield" . Data.Aeson.Object . yieldProperties
  toEncoding = pairs . yieldProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON Yield where
  parseJSON = withObject "Yield" $ \o ->
    Yield <$> o .: "data" <*> o .: "provenance" <*> o .:? "source"

instance SizeOf Yield where
  sizeOf (Yield dataYield prov _) =
    (constructorCost 2) + (sizeOf dataYield) + (sizeOf prov)

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

instance Pretty PactContinuation where
  pretty (PactContinuation d as) = parensSep (pretty d:map pretty as)

instance NFData PactContinuation

pactContinuationProperties :: JsonProperties PactContinuation
pactContinuationProperties o =
  [ "args" .= _pcArgs o
  , "def" .= _pcDef o
  ]
{-# INLINE pactContinuationProperties #-}

instance ToJSON PactContinuation where
  toJSON = enableToJSON "Pact.Types.Continuation.PactContinuation" . lensyToJSON 3
  toEncoding = pairs . mconcat . pactContinuationProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON PactContinuation where parseJSON = lensyParseJSON 3

-- | Result of evaluation of a 'defpact'.
--
data PactExec = PactExec
  { _peStepCount :: Int
    -- ^ Count of steps in pact (discovered when code is executed)
  , _peYield :: !(Maybe Yield)
    -- ^ Yield value if invoked
  , _peExecuted :: Maybe Bool
    -- ^ Only populated for private pacts, indicates if step was executed or skipped.
  , _peStep :: Int
    -- ^ Step that was executed or skipped
  , _pePactId :: PactId
    -- ^ Pact id. On a new pact invocation, is copied from tx id.
  , _peContinuation :: PactContinuation
    -- ^ Strict (in arguments) application of pact, for future step invocations.
  , _peStepHasRollback :: !Bool
    -- ^ Track whether a current step has a rollback
  } deriving (Eq, Show, Generic)

instance NFData PactExec

pactExecProperties :: JsonProperties PactExec
pactExecProperties o =
  [ "executed" .= _peExecuted o
  , "pactId" .= _pePactId o
  , "stepHasRollback" .= _peStepHasRollback o
  , "step" .= _peStep o
  , "yield" .= _peYield o
  , "continuation" .= _peContinuation o
  , "stepCount" .= _peStepCount o
  ]
{-# INLINE pactExecProperties #-}

instance ToJSON PactExec where
  toJSON = enableToJSON "Pact.Types.Continuation.PactExec" . lensyToJSON 3
  toEncoding = pairs . mconcat . pactExecProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance FromJSON PactExec where parseJSON = lensyParseJSON 3
instance Pretty PactExec where pretty = viaShow


makeLenses ''PactExec
makeLenses ''PactStep
makeLenses ''PactContinuation
makeLenses ''Yield
makeLenses ''Provenance
