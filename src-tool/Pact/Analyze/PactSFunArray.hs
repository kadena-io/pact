{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This is the old 'SFunArray' which has since been removed from SBV (see
-- https://github.com/LeventErkok/sbv/commit/77858f34f98bb233b86ed3a9f79f75a3707c9400),
-- until we have time to move to the new implementation:
module Pact.Analyze.PactSFunArray
  ( PactSFunArray
  , mkPactSFunArray
  , eitherArray
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.SBV
import           Data.SBV.Internals     (SymArray (..), registerKind)

-- | Declare a new functional symbolic array. Note that a read from an
-- uninitialized cell will result in an error.
declNewSFunArray
  :: forall a b m.
     (HasKind a, HasKind b, MonadSymbolic m)
  => Maybe String
  -> Maybe (SBV b)
  -> m (PactSFunArray a b)
declNewSFunArray mbNm mDef = do
  st <- symbolicEnv
  liftIO $ mapM_ (registerKind st) [kindOf (undefined :: a), kindOf (undefined :: b)]
  return $ PactSFunArray handleNotFound
  where handleNotFound = case mDef of
          Nothing  -> error . msg mbNm
          Just def -> const def

        msg Nothing   i = "Reading from an uninitialized array entry, index: " ++ show i
        msg (Just nm) i = "Array " ++ show nm ++ ": Reading from an uninitialized array entry, index: " ++ show i

newtype PactSFunArray a b = PactSFunArray (SBV a -> SBV b)

instance (HasKind a, HasKind b) => Show (PactSFunArray a b) where
  show (PactSFunArray _) = "PactSFunArray<" ++ showType (undefined :: a) ++ ":" ++ showType (undefined :: b) ++ ">"

-- | Lift a function to an array. Useful for creating arrays in a pure context. (Otherwise use `newArray`.)
-- A simple way to create an array such that reading an unintialized value is assigned a free variable is
-- to simply use an uninterpreted function. That is, use:
--
--  @ mkPactSFunArray (uninterpret "initial") @
--
-- Note that this will ensure all uninitialized reads to the same location will return the same value,
-- without constraining them otherwise; with different indexes containing different values.
mkPactSFunArray :: (SBV a -> SBV b) -> PactSFunArray a b
mkPactSFunArray = PactSFunArray

-- SFunArrays are only "Mergeable". Although a brute
-- force equality can be defined, any non-toy instance
-- will suffer from efficiency issues; so we don't define it
instance SymArray PactSFunArray where
  newArray nm                                 = declNewSFunArray (Just nm)
  newArray_                                   = declNewSFunArray Nothing
  readArray     (PactSFunArray f)                 = f
  writeArray    (PactSFunArray f) a b             = PactSFunArray (\a' -> ite (a .== a') b (f a'))
  mergeArrays t (PactSFunArray g)   (PactSFunArray h) = PactSFunArray (\x -> ite t (g x) (h x))
  newArrayInState                             = error "PactSFunArray: newArrayInState is not implemented"
  sListArray                                  = error "PactSFunArray: sListArray is not implemented"

instance SymVal b => Mergeable (PactSFunArray a b) where
  symbolicMerge _ = mergeArrays

eitherArray :: PactSFunArray a Bool -> PactSFunArray a Bool -> PactSFunArray a Bool
eitherArray (PactSFunArray g) (PactSFunArray h) = PactSFunArray (\x -> g x .|| h x)
