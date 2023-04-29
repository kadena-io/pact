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
  , NestedPactExec(..)
  , Yield(..)
  , Provenance(..)
    -- * Optics
  , peStepCount, peYield, peExecuted, pePactId
  , peStep, peContinuation, peStepHasRollback
  , npeStepCount, npeYield, npeExecuted, npeStep
  , npePactId, npeContinuation, npeNested
  , psStep, psRollback, psPactId, psResume, peNested
  , pcDef, pcArgs
  , yData, yProvenance, ySourceChain
  , pTargetChainId, pModuleHash
  , toNestedPactExec
  , fromNestedPactExec
  ) where

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Control.Lens hiding ((.=))

import Data.Aeson
import Data.Map.Strict(Map)
import Data.Maybe(fromMaybe)
import qualified Data.Map.Strict as Map

import Test.QuickCheck

import Pact.Types.ChainId (ChainId)
import Pact.Types.PactValue
import Pact.Types.PactValue.Arbitrary ()
import Pact.Types.Pretty
import Pact.Types.SizeOf
import Pact.Types.Term
import Pact.Types.Util (lensyToJSON, lensyParseJSON, JsonProperties, JsonMProperties, enableToJSON, (.?=))
import Pact.JSON.Legacy.Value
import qualified Pact.JSON.Legacy.HashMap as LHM

import qualified Pact.JSON.Encode as J

-- | Provenance datatype contains all of the necessary
-- data to 'endorse' a yield object.
--
data Provenance = Provenance
  { _pTargetChainId :: !ChainId
    -- ^ the target chain id for the endorsement
  , _pModuleHash :: !ModuleHash
    -- ^ a hash of current containing module
  } deriving (Eq, Show, Generic)

instance Arbitrary Provenance where
  arbitrary = Provenance <$> arbitrary <*> arbitrary

instance Pretty Provenance where
  pretty (Provenance c h) = "(chain = \"" <> pretty c <> "\" hash=\"" <> pretty h <> "\")"

instance SizeOf Provenance where
  sizeOf ver (Provenance chainId modHash) =
    (constructorCost 2) + (sizeOf ver chainId) + (sizeOf ver modHash)

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

instance J.Encode Provenance where
  build o = J.object
    [ "targetChainId" J..= _pTargetChainId o
    , "moduleHash" J..= _pModuleHash o
    ]
  {-# INLINE build #-}

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
  arbitrary = Yield
    <$> resize 10 arbitrary
    <*> frequency
      [ (1, pure Nothing)
      , (4, Just <$> arbitrary)
      ]
    <*> pure Nothing

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

instance J.Encode Yield where
  build o = J.object
    [ "data" J..= _yData o
    , "source" J..?= _ySourceChain o
    , "provenance" J..= _yProvenance o
    ]
  {-# INLINE build #-}

instance FromJSON Yield where
  parseJSON = withObject "Yield" $ \o ->
    Yield <$> o .: "data" <*> o .: "provenance" <*> o .:? "source"

instance SizeOf Yield where
  sizeOf ver (Yield dataYield prov _) =
    (constructorCost 2) + (sizeOf ver dataYield) + (sizeOf ver prov)

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
  { _pcDef :: !Name
  , _pcArgs :: ![PactValue]
  } deriving (Eq, Show, Generic)

instance Pretty PactContinuation where
  pretty (PactContinuation d as) = parensSep (pretty d:map pretty as)

instance NFData PactContinuation

instance Arbitrary PactContinuation where
  arbitrary = PactContinuation <$> arbitrary <*> resize 10 arbitrary

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

instance J.Encode PactContinuation where
  build o = J.object
    [ "args" J..= J.Array (_pcArgs o)
    , "def" J..= _pcDef o
    ]
  {-# INLINE build #-}


instance FromJSON PactContinuation where parseJSON = lensyParseJSON 3

-- | Result of evaluation of a 'defpact'.
--
data PactExec = PactExec
  { _peStepCount :: !Int
    -- ^ Count of steps in pact (discovered when code is executed)
  , _peYield :: !(Maybe Yield)
    -- ^ Yield value if invoked
  , _peExecuted :: !(Maybe Bool)
    -- ^ Only populated for private pacts, indicates if step was executed or skipped.
  , _peStep :: !Int
    -- ^ Step that was executed or skipped
  , _pePactId :: !PactId
    -- ^ Pact id. On a new pact invocation, is copied from tx id.
  , _peContinuation :: !PactContinuation
    -- ^ Strict (in arguments) application of pact, for future step invocations.
  , _peStepHasRollback :: !Bool
    -- ^ Track whether a current step has a rollback
  , _peNested :: !(Map PactId NestedPactExec)
    -- ^ Track whether a current step has nested defpact evaluation results
  } deriving (Eq, Show, Generic)

instance NFData PactExec

instance Arbitrary PactExec where
  arbitrary = PactExec
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> resize 2 arbitrary

pactExecProperties
  :: ToJSON x
  => (Map PactId NestedPactExec -> x)
    -- ^ adjust ordering of properties in the map for legacy JSON encoding
  -> JsonProperties PactExec
pactExecProperties f o =
    [ "nested" .= f (_peNested o) | not (Map.null (_peNested o))]
    ++
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
  toJSON = enableToJSON "Pact.Types.Continuation.PactExec" . object . pactExecProperties id
  toEncoding = pairs . mconcat . pactExecProperties legacyMap
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode PactExec where
  build o = J.object
    [ "nested" J..?= J.ifMaybe (not . LHM.null) (legacyMap (_peNested o))
    , "executed" J..= _peExecuted o
    , "pactId" J..= _pePactId o
    , "stepHasRollback" J..= _peStepHasRollback o
    , "step" J..= J.Aeson (_peStep o)
    , "yield" J..= _peYield o
    , "continuation" J..= _peContinuation o
    , "stepCount" J..= J.Aeson (_peStepCount o)
    ]
  {-# INLINE build #-}

instance FromJSON PactExec where
  parseJSON = withObject "PactExec" $ \o ->
    PactExec
      <$> o .: "stepCount"
      <*> o .: "yield"
      <*> o .: "executed"
      <*> o .: "step"
      <*> o .: "pactId"
      <*> o .: "continuation"
      <*> o .: "stepHasRollback"
      <*> (fromMaybe mempty <$> o .:? "nested")

instance Pretty PactExec where pretty = viaShow

data NestedPactExec = NestedPactExec
  { _npeStepCount :: !Int
    -- ^ Count of steps in pact (discovered when code is executed)
  , _npeYield :: !(Maybe Yield)
    -- ^ Yield value if invoked
  , _npeExecuted :: !(Maybe Bool)
    -- ^ Only populated for private pacts, indicates if step was executed or skipped.
  , _npeStep :: !Int
    -- ^ Step that was executed or skipped
  , _npePactId :: !PactId
    -- ^ Pact id. On a new pact invocation, is copied from tx id.
  , _npeContinuation :: !PactContinuation
    -- ^ Strict (in arguments) application of pact, for future step invocations.
  , _npeNested :: !(Map PactId NestedPactExec)
    -- ^ Track whether a current step has nested defpact evaluation results
  } deriving (Eq, Show, Generic)

toNestedPactExec :: PactExec -> NestedPactExec
toNestedPactExec (PactExec stepCount yield exec step pid cont _ nested) =
  NestedPactExec stepCount yield exec step pid cont nested

fromNestedPactExec :: Bool -> NestedPactExec -> PactExec
fromNestedPactExec rollback (NestedPactExec stepCount yield exec step pid cont nested) =
  PactExec stepCount yield exec step pid cont rollback nested

nestedPactExecProperties
  :: ToJSON x
  => (Map PactId NestedPactExec -> x)
    -- ^ adjust ordering of properties in the map for legacy JSON encoding
  -> JsonProperties NestedPactExec
nestedPactExecProperties f o =
  [ "nested" .= f (_npeNested o)
  , "executed" .= _npeExecuted o
  , "pactId" .= _npePactId o
  , "step" .= _npeStep o
  , "yield" .= _npeYield o
  , "continuation" .= _npeContinuation o
  , "stepCount" .= _npeStepCount o
  ]
{-# INLINE nestedPactExecProperties #-}

instance ToJSON NestedPactExec where
  toJSON = enableToJSON "Pact.Types.Continuation.NestedPactExec" . object . nestedPactExecProperties id
  toEncoding = pairs. mconcat . nestedPactExecProperties legacyMap
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}

instance J.Encode NestedPactExec where
  build o = J.object
    [ "nested" J..= legacyMap (_npeNested o)
    , "executed" J..= _npeExecuted o
    , "pactId" J..= _npePactId o
    , "step" J..= J.Aeson (_npeStep o)
    , "yield" J..= _npeYield o
    , "continuation" J..= _npeContinuation o
    , "stepCount" J..= J.Aeson (_npeStepCount o)
    ]
  {-# INLINE build #-}

instance FromJSON NestedPactExec where
  parseJSON = withObject "NestedPactExec" $ \o ->
    NestedPactExec
      <$> o .: "stepCount"
      <*> o .: "yield"
      <*> o .: "executed"
      <*> o .: "step"
      <*> o .: "pactId"
      <*> o .: "continuation"
      <*> o .: "nested"
instance NFData NestedPactExec
instance Pretty NestedPactExec where pretty = viaShow

instance Arbitrary NestedPactExec where
  arbitrary = NestedPactExec
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> do
      Positive k <- arbitrary
      scale (`div` (k + 3)) arbitrary

makeLenses ''PactExec
makeLenses ''NestedPactExec
makeLenses ''PactStep
makeLenses ''PactContinuation
makeLenses ''Yield
makeLenses ''Provenance
