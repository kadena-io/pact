module Pact.Core.Pacts.Types
 ( PactId(..)
 , PactContinuation(..)
 , PactStep(..)
 , ChainId(..)
 ) where

-- Todo: yield
import Pact.Types.Term(PactId(..))
import Pact.Types.ChainId
import Pact.Types.Continuation(PactStep(..))

data PactContinuation name v
  = PactContinuation
  { _pcName :: name
  , _pcArgs :: [v]
  } deriving (Eq, Show)
