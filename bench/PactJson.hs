{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: PactJson
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- benchmarks for pact-json
--
module Main
( main
) where

import Bound

import Criterion
import Criterion.Main

import qualified Data.Aeson as A
#ifdef PACT_TOJSON
import qualified Data.Aeson.Encoding as A
#endif
import qualified Data.ByteString.Lazy as BL

import qualified Pact.JSON.Encode as J

import System.IO.Unsafe

import Test.QuickCheck

-- internal modules
import Pact.Types.Command
import Pact.Types.Names
import Pact.Types.RowData
import Pact.Types.SPV
import Pact.Types.Term
import Pact.Types.Term.Arbitrary ()


-- -------------------------------------------------------------------------- --
-- Main

main :: IO ()
main = defaultMain [benchmarks]

-- -------------------------------------------------------------------------- --
-- Benchmarks

benchmarks :: Benchmark
benchmarks = bgroup "JsonEncoding"
  [ bgroup "pact"
    [ group "module" pactModule
    , group "RowData" rowData
    , group "CommandResult" commandResult
    , group "ContProof" contProof
    ]
  ]

group
  :: J.Encode a
#ifdef PACT_TOJSON
  => A.ToJSON a
#endif
  => String
  -> a
  -> Benchmark
group l a = bgroup l
  [ bench_encode a
#ifdef PACT_TOJSON
  , bench_toJSON a
  , bench_toEncoding a
#endif
  ]

-- -------------------------------------------------------------------------- --
-- Benchmark Functions

#ifdef PACT_TOJSON
bench_toJSON :: A.ToJSON a => a -> Benchmark
bench_toJSON a = bench "bench_toJSON" $ nf run_toJSON a

run_toJSON :: A.ToJSON a => a -> BL.ByteString
run_toJSON x = A.encode (A.toJSON x)
{-# NOINLINE run_toJSON #-}

bench_toEncoding :: A.ToJSON a => a -> Benchmark
bench_toEncoding a = bench "bench_toEncoding" $ nf run_toEncoding a

run_toEncoding :: A.ToJSON a => a -> BL.ByteString
run_toEncoding = A.encodingToLazyByteString . A.toEncoding
{-# NOINLINE run_toEncoding #-}
#endif

bench_encode :: J.Encode a => a -> Benchmark
bench_encode a = bench "bench_encode" $ nf run_encode a

run_encode :: J.Encode a => a -> BL.ByteString
run_encode = J.encode
{-# NOINLINE run_encode #-}

-- -------------------------------------------------------------------------- --
-- Orphans

deriving newtype instance A.ToJSON a => A.ToJSON (J.Aeson a)
deriving newtype instance A.FromJSON a => A.FromJSON (J.Aeson a)
deriving newtype instance Show a => Show (J.Aeson a)
deriving newtype instance Eq a => Eq (J.Aeson a)
deriving newtype instance Arbitrary a => Arbitrary (J.Aeson a)
deriving instance Functor J.Aeson
deriving instance Foldable J.Aeson
deriving instance Traversable J.Aeson

instance J.Encode [J.Aeson ()] where
  build a = J.build $ J.Array a

instance J.Encode [Var Int [J.Aeson ()]] where
  build a = J.build $ J.Array a

-- -------------------------------------------------------------------------- --
-- Benchmark Data
--

type A = J.Aeson

pactModule :: Module Name
pactModule = unsafeDupablePerformIO $ generate $ arbitrary
{-# NOINLINE pactModule #-}

rowData :: RowData
rowData = unsafeDupablePerformIO $ generate $ arbitrary
{-# NOINLINE rowData #-}

commandResult :: CommandResult (A ())
commandResult = unsafeDupablePerformIO $ generate $ arbitrary
{-# NOINLINE commandResult #-}

contProof :: ContProof
contProof = unsafeDupablePerformIO $ generate $ arbitrary
{-# NOINLINE contProof #-}

