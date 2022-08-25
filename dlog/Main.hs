{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Control.Monad.Trans.Random.Strict
import Data.Text(Text)
import qualified Data.Text as T
import Numeric
import System.Random

import Pact.Gas.Table
import Pact.Types.SizeOf
-- import Pact.Types.Util
-- import Pact.Types.Term
import Pact.Repl.Types
import Pact.Repl
import Pact.Types.PactValue
import Pact.Types.Pretty


main :: IO ()
main = do
  d <- genDouble
  print d

intGenTest :: IO ()
intGenTest = forM_ [-100000000000000000000000000000000 .. 1000000000000000000000000000000000] $ \i -> print [fromIntegral (sizeOf i), intCost i]

dtt :: Double -> Text
dtt i = T.pack $ showFFloat Nothing i ""

pactTx :: Double -> Double -> Text
pactTx i j = T.unlines
  [ "(env-gasmodel \"table\")"
  , "(env-gaslimit 150000000)"
  , T.unwords ["[(*", dtt i, dtt j, ")", "(exp", dtt i , ")", "(sqrt", dtt (abs j), ") (env-gas)]"]
  ]

genDouble :: IO [String]
genDouble = flip evalRandT (mkStdGen 112858595496370454) $ replicateM 1000000 $ do
  i <- getRandomR (-100000000000000000000000000000000 :: Double, 1000000000000000000000000000000000)
  j <- getRandomR (-100000000000000000000000000000000 :: Double, 1000000000000000000000000000000000)
  let ptx = pactTx i j
  either id (either T.unpack renderCompactString . toPactValue) <$> liftIO (evalRepl StringEval (T.unpack ptx))

