{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for representing capabilities in analysis.
module Pact.Analyze.Types.Capability where

import           Control.Lens                 ((<&>), makeLenses)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.SBV                     (Mergeable(..), sFalse)
import           Data.Type.Equality           ((:~:) (Refl))

import           Pact.Analyze.LegacySFunArray (eitherArray, mkSFunArray)
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Types.Types

data Capability where
  Capability :: SingList schema -> CapName -> Capability

instance Show Capability where
  showsPrec p (Capability sch capName) = showParen (p > 10) $
      showString "Capability "
    . showsPrec 11 sch
    . showChar ' '
    . showsPrec 11 capName

newtype CapabilityGrants
  = CapabilityGrants { _capabilityGrants :: Map CapName (EKeySFunArray Bool) }
  deriving Show

mkCapabilityGrants :: [Capability] -> CapabilityGrants
mkCapabilityGrants caps = CapabilityGrants $ Map.fromList $
  caps <&> \(Capability schema name) ->
    ( name
    , EKeySFunArray (SObjectUnsafe schema) (mkSFunArray $ const sFalse)
    )

extendGrants :: CapabilityGrants -> CapabilityGrants -> CapabilityGrants
extendGrants (CapabilityGrants newGrants) (CapabilityGrants oldGrants) =
    -- We can intersect because both maps will contain the exact same keys
    CapabilityGrants $ Map.intersectionWith eitherContains newGrants oldGrants

  where
    eitherContains :: EKeySFunArray Bool -> EKeySFunArray Bool -> EKeySFunArray Bool
    eitherContains (EKeySFunArray ty1 arr1) (EKeySFunArray ty2 arr2) =
      case singEq ty1 ty2 of
        Just Refl -> EKeySFunArray ty1 $ eitherArray arr1 arr2
        Nothing   -> error $
          "extendGrants: type mismatch: " ++ show ty1 ++ " vs " ++ show ty2

instance Semigroup CapabilityGrants where
  (<>) = extendGrants

instance Mergeable CapabilityGrants where
  symbolicMerge f t (CapabilityGrants left) (CapabilityGrants right) =
    -- Using intersectionWith here is fine, because we know that each map has
    -- all possible capabilities:
    CapabilityGrants $ Map.intersectionWith (symbolicMerge f t) left right

-- | The index into the family that is a 'Capability'.
data Token where
  Token :: SingList schema -> CapName -> S (ConcreteObj schema) -> Token

makeLenses ''CapabilityGrants
