{-# language TypeApplications #-}

module Main (main) where

import Control.Monad.State

import Data.Foldable
import Data.Hash.Blake2
import qualified Data.Vector as V
import Data.Word

import Pact.Native.Ops

import System.Environment
import qualified System.Random.MWC as R

defaultSeeds :: [Word32]
defaultSeeds =
    [ 589745628
    , 3614188581
    , 2379548895
    , 2444956536
    , 3025838153
    , 2187946641
    , 2619845360
    , 3665997119
    , 4285837746
    , 2283878998
    , 3395806766
    , 1662954663
    , 4001835075
    , 600188384
    , 3670054037
    , 2138002762
    , 1591324870
    , 3097839710
    , 2854492958
    , 3473084507
    , 1641925865
    , 1753880448
    , 42284980
    , 233524998
    , 1002107799
    , 4179284584
    , 3788126726
    , 525293318
    , 4042855066
    , 2829867043
    , 91626930
    , 1886364755
    ]

testCount = 100000

main = do
    args <- getArgs
    seeds <-
        if null args
        then return defaultSeeds
        else read <$> getContents
    traverse_ (\s -> testWithGen =<< R.initialize (V.replicate 258 s)) seeds
    where
    testWithGen g = do
        hashCtx <- initialize @Blake2b512
        replicateM testCount $ do
            (d1, d2) <- (,) <$> R.uniform g <*> R.uniform g
            updateStorable @Blake2b512 hashCtx $ c'c_pow d1 d2
        hash <- finalize @Blake2b512 hashCtx
        print hash