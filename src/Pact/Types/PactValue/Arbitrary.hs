{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Pact.Types.PactValue.Arbitrary
(
-- * Helper functions for generating arbitrary pact values
  PactValueGeneratorSize(..)
, decreaseGenSize
, genSomePactValue
, genTerminatingPactValueGuard
, genPactValueObjectMap
, genPactValueList
, genPactValueGuard
, genUserGuard
) where

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Test.QuickCheck

-- internal modules

import Pact.Types.PactValue
import Pact.Types.Term
import Pact.Types.Term.Arbitrary ()

-- -------------------------------------------------------------------------- --
--

instance Arbitrary PactValue where
  arbitrary = genSomePactValue RecurseTwice

-- -------------------------------------------------------------------------- --
-- Helper Functions

-- | Determines how deep a generated PactValue _could_ be.
-- Restricts how many times a recursive PactValue constructor (i.e. PObject,
-- PList, and PGuard) can be called when creating a PactValue.
--
genSomePactValue :: PactValueGeneratorSize -> Gen PactValue
genSomePactValue TerminateFast = oneof
  [ PLiteral <$> arbitrary
  , PGuard <$> oneof genTerminatingPactValueGuard
  ]
genSomePactValue s = oneof
  [ PLiteral <$> arbitrary
  , PList <$> genPactValueList (decreaseGenSize s)
  , PObject <$> genPactValueObjectMap (decreaseGenSize s)
  , PGuard <$> genPactValueGuard (decreaseGenSize s)
  ]

data PactValueGeneratorSize = TerminateFast | RecurseOnce | RecurseTwice

decreaseGenSize :: PactValueGeneratorSize -> PactValueGeneratorSize
decreaseGenSize TerminateFast = TerminateFast -- lowest value
decreaseGenSize RecurseOnce = TerminateFast
decreaseGenSize RecurseTwice = RecurseOnce

genTerminatingPactValueGuard :: [Gen (Guard PactValue)]
genTerminatingPactValueGuard =
  [ GPact <$> arbitrary
  , GKeySet <$> arbitrary
  , GKeySetRef <$> arbitrary
  , GModule <$> arbitrary
  ]

genPactValueObjectMap :: PactValueGeneratorSize -> Gen (ObjectMap PactValue)
genPactValueObjectMap genSize = ObjectMap . M.fromList <$> genMap
 where
  genOneKeyValue = do
    f <- arbitrary :: Gen FieldKey
    pv <- genSomePactValue genSize
    pure (f, pv)
  genMap = listOf1 genOneKeyValue

genPactValueList :: PactValueGeneratorSize -> Gen (V.Vector PactValue)
genPactValueList genSize = V.fromList <$> listOf1 (genSomePactValue genSize)

genPactValueGuard :: PactValueGeneratorSize -> Gen (Guard PactValue)
genPactValueGuard genSize = oneof (genTerminatingPactValueGuard <> [genUserGuard genSize])

genUserGuard :: PactValueGeneratorSize -> Gen (Guard PactValue)
genUserGuard genSize = do
  args <- listOf1 (genSomePactValue genSize)
  fun <- arbitraryName (0,1,1,0)
  pure $ GUser $ UserGuard fun args

