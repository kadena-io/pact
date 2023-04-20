{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- |
-- Module      :  Pact.Trans.Types
-- Copyright   :  (C) 2023 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- Operators and math built-ins based on the MPFR library.
--

module Pact.Trans.Types
  ( TransResult(..)
  , doubleToTransResult
  ) where

data TransResult a
  = TransNumber !a
  | TransNaN !a
  | TransInf !a
  | TransNegInf !a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

doubleToTransResult :: Double -> TransResult Double
doubleToTransResult r
  | isNaN r = TransNaN r
  | isInfinite r && r < 0 = TransNegInf r
  | isInfinite r = TransInf r
  | otherwise = TransNumber r

