{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pact.Core.Pacts.Types
 ( PactId(..)
 , PactContinuation(..)
 , ChainId(..)
 ) where

-- Todo: yield
import Data.Text(Text)
import Pact.Core.Pretty

newtype ChainId
  = ChainId { _chainId :: Text }
  deriving (Eq,Ord,Show,Pretty)

newtype PactId
  = PactId Text
  deriving (Eq,Ord,Show,Pretty)

data PactContinuation name v
  = PactContinuation
  { _pcName :: name
  , _pcArgs :: [v]
  } deriving (Eq, Show)
