{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Test.QuickCheck
import qualified Data.Text as T

import Pact.Gas.Table
import Pact.Types.SizeOf
import Pact.Types.Util
import Pact.Types.Term
import Pact.Repl.Types
import Pact.Repl


main :: IO ()
main = do
  d <- generate $ genDouble
  sequence d >>= print

intGenTest :: IO ()
intGenTest = forM_ [-1000000000000000000000 .. 10000000000000000000000] $ \i -> print [fromIntegral (sizeOf i), intCost i]

dtt i = T.pack (show i)

pactTx :: Double -> Double -> T.Text
pactTx i j = T.unlines
  [ "(env-gasmodel \"table\")"
  , "(env-gaslimit 150000000)"
  , T.unwords ["[(*", dtt i, dtt j, ")", "(exp", dtt i , ")", "(sqrt", dtt j, ") (env-gas)]"]
  ]

genDouble :: Gen [IO (Either String (Term Name))]
genDouble = variant 57274812 $ listOf1 $ do
  i <- choose (-1000000000000000000000, 10000000000000000000000)
  j <- choose (-1000000000000000000000, 10000000000000000000000)
  let ptx = pactTx i j
  pure (evalRepl StringEval (T.unpack ptx))

