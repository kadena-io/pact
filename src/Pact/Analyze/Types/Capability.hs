{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for representing capabilities in analysis. Whereas colloquially in
-- Pact we talk about "granting capabilities", in our symbolic modeling we
-- speak more precisely of "granting tokens", where a token is effectively the
-- combination of the name of a capability and a set of arguments to it.
module Pact.Analyze.Types.Capability where

import           Control.Lens                 ((<&>), makeLenses)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.SBV                     (Mergeable(..), sFalse)
import           Data.Type.Equality           ((:~:) (Refl))

import           Pact.Types.Pretty

import           Pact.Analyze.LegacySFunArray (eitherArray, mkSFunArray)
import           Pact.Analyze.Types.Shared
import           Pact.Analyze.Types.Types

-- | The "signature" for a capability in Pact -- a family of tokens.
data Capability where
  Capability :: SingList schema -> CapName -> Capability

instance Show Capability where
  showsPrec p (Capability sch capName) = showParen (p > 10) $
      showString "Capability "
    . showsPrec 11 sch
    . showChar ' '
    . showsPrec 11 capName

instance Pretty Capability where
  pretty (Capability _ (CapName cn)) = pretty cn

-- | The index into the family that is a 'Capability'. Think of this of the
-- arguments to a particular call of a capability.
data Token where
  Token :: SingList schema -> CapName -> S (ConcreteObj schema) -> Token

-- | Whether any token is currently granted, organized by capability name. The
-- keys in this map are statically determined by the number of @defcap@s in the
-- module under analysis.
newtype TokenGrants
  = TokenGrants { _capabilityGrants :: Map CapName (EKeySFunArray Bool) }
  deriving Show

mkTokenGrants :: [Capability] -> TokenGrants
mkTokenGrants caps = TokenGrants $ Map.fromList $
  caps <&> \(Capability schema name) ->
    ( name
    , EKeySFunArray (SObjectUnsafe schema) (mkSFunArray $ const sFalse)
    )

extendGrants :: TokenGrants -> TokenGrants -> TokenGrants
extendGrants (TokenGrants newGrants) (TokenGrants oldGrants) =
    -- We can intersect because both maps will contain the exact same keys
    TokenGrants $ Map.intersectionWith eitherContains newGrants oldGrants

  where
    eitherContains :: EKeySFunArray Bool -> EKeySFunArray Bool -> EKeySFunArray Bool
    eitherContains (EKeySFunArray ty1 arr1) (EKeySFunArray ty2 arr2) =
      case singEq ty1 ty2 of
        Just Refl -> EKeySFunArray ty1 $ eitherArray arr1 arr2
        Nothing   -> error $
          "extendGrants: type mismatch: " ++ show ty1 ++ " vs " ++ show ty2

instance Semigroup TokenGrants where
  (<>) = extendGrants

instance Mergeable TokenGrants where
  symbolicMerge f t (TokenGrants left) (TokenGrants right) =
    -- Using intersectionWith here is fine, because we know that each map has
    -- all possible capabilities in the module.
    TokenGrants $ Map.intersectionWith (symbolicMerge f t) left right

makeLenses ''TokenGrants
