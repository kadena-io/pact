{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Pact.Types.Continuation
  ( -- * data
    PactStep(..)
  , PactContinuation(..)
  , Yield(..)
  , ChainId(..)
    -- * optics
  , psStep, psRollback, psPactId, psResume
  , pcDef, pcArgs
  , yData, yTarget, yEndorsement
  , chainId
  )where


import GHC.Generics

import Control.Lens (Lens', makeLenses, lens)

import Data.String
import Data.Text (Text)

import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Term

newtype ChainId = ChainId { _chainId :: Text }
  deriving (Eq, Ord, Show, IsString, Generic)

chainId :: Lens' ChainId Text
chainId = lens _chainId (\_ t -> ChainId t)

-- | Environment setup for pact execution.
data PactStep = PactStep
  { _psStep :: !Int
  , _psRollback :: !Bool
  , _psPactId :: !PactId
  , _psResume :: !(Maybe (ObjectMap (Term Name)))
  } deriving (Eq, Show)

data PactContinuation = PactContinuation
  { _pcDef :: Def Ref
  , _pcArgs :: [PactValue]
  } deriving (Eq, Show)

data Yield = Yield
  { _yData :: Object Name
  , _yTarget :: ChainId
  , _yEndorsement :: Hash
  } deriving (Eq, Show)


makeLenses ''PactStep
makeLenses ''PactContinuation
makeLenses ''Yield
