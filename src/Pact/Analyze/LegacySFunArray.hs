{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is the old 'SFunArray' which has since been removed from SBV (see
-- https://github.com/LeventErkok/sbv/commit/77858f34f98bb233b86ed3a9f79f75a3707c9400),
-- until we have time to move to the new implementation:

module Pact.Analyze.LegacySFunArray
  ( SFunArray
  , mkSFunArray
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask)
import           Data.SBV               hiding (SFunArray)
import           Data.SBV.Internals     (registerKind)

-- | Declare a new functional symbolic array. Note that a read from an uninitialized cell will result in an error.
declNewSFunArray :: forall a b. (HasKind a, HasKind b) => Maybe String -> Symbolic (SFunArray a b)
declNewSFunArray mbNm = do st <- ask
                           liftIO $ mapM_ (registerKind st) [kindOf (undefined :: a), kindOf (undefined :: b)]
                           return $ SFunArray $ error . msg mbNm
  where msg Nothing   i = "Reading from an uninitialized array entry, index: " ++ show i
        msg (Just nm) i = "Array " ++ show nm ++ ": Reading from an uninitialized array entry, index: " ++ show i

newtype SFunArray a b = SFunArray (SBV a -> SBV b)

instance (HasKind a, HasKind b) => Show (SFunArray a b) where
  show (SFunArray _) = "SFunArray<" ++ showType (undefined :: a) ++ ":" ++ showType (undefined :: b) ++ ">"

-- | Lift a function to an array. Useful for creating arrays in a pure context. (Otherwise use `newArray`.)
-- A simple way to create an array such that reading an unintialized value is assigned a free variable is
-- to simply use an uninterpreted function. That is, use:
--
--  @ mkSFunArray (uninterpret "initial") @
--
-- Note that this will ensure all uninitialized reads to the same location will return the same value,
-- without constraining them otherwise; with different indexes containing different values.
mkSFunArray :: (SBV a -> SBV b) -> SFunArray a b
mkSFunArray = SFunArray

-- SFunArrays are only "Mergeable". Although a brute
-- force equality can be defined, any non-toy instance
-- will suffer from efficiency issues; so we don't define it
instance SymArray SFunArray where
  newArray nm                                 = declNewSFunArray (Just nm)
  newArray_                                   = declNewSFunArray Nothing
  readArray     (SFunArray f)                 = f
  writeArray    (SFunArray f) a b             = SFunArray (\a' -> ite (a .== a') b (f a'))
  mergeArrays t (SFunArray g)   (SFunArray h) = SFunArray (\x -> ite t (g x) (h x))
  newArrayInState                             = undefined

instance SymWord b => Mergeable (SFunArray a b) where
  symbolicMerge _ = mergeArrays
