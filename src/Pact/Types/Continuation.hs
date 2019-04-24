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
  , Yield(..)
    -- * combinators
  , endorse
    -- * optics
  , psStep, psRollback, psPactId, psResume
  , pcDef, pcArgs
  , yData, yTarget, yEndorsement
  )where


import Control.Lens

import Data.Aeson (encode)
import Data.ByteString.Lazy (toStrict)
import Data.Semigroup ((<>))

import Pact.Types.ChainMeta (ChainId)
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Term


-- | The type of a set of yielded values of a pact step.
-- Endorsement hashes ('_yEndorsement') are a combination of
-- the following data:
--
-- 1. The hash of the data '_yData'
-- 2. The hash of the target chain '_yTarget'
-- 3. The executing pact id
-- 4. The hash of the defining module
data Yield = Yield
  { _yData :: !(Object Name)
  , _yTarget :: !ChainId
  , _yEndorsement :: !Hash
  } deriving (Eq, Show)

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
endorse
  :: Hash
  -- ^ the hash of the containing module for a pact
  -> PactId
  -- ^ the executing pact id
  -> Object Name
  -- ^ yield data
  -> ChainId
  -- ^ target chain id
  -> Hash
endorse (Hash mh) pid d tid =
  toUntypedHash . hash @('Blake2b_256) $!
    mh <> toStrict (encode pid <> encode d <> encode tid)

-- | Environment setup for pact execution.
data PactStep = PactStep
  { _psStep :: !Int
  , _psRollback :: !Bool
  , _psPactId :: !PactId
  , _psResume :: !(Maybe Yield)
  } deriving (Eq, Show)

-- | The type of pact continuations (i.e. defpact)
data PactContinuation = PactContinuation
  { _pcDef :: Def Ref
  , _pcArgs :: [PactValue]
  } deriving (Eq, Show)


makeLenses ''PactStep
makeLenses ''PactContinuation
makeLenses ''Yield
