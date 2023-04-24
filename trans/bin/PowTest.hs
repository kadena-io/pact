{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Decimal
import qualified Pact.Trans.Musl as Musl
import qualified Pact.Trans.Dec as Dec
import qualified Pact.Trans.Dbl as Dbl
import Pact.Trans.Types

x :: Decimal
x = 15.034465284692086701747761395233132973944448512421004399685858401206740385711739229018307610943234609057822959334669087436253689423614206061665462283698768757790600552385430913941421707844383369633809803959413869974997415115322843838226312287673293352959835

y :: Decimal
y = 3.466120406090666777582519661568003549307295836842780244500133445635634490670936927006970368136648330889718447039413255137656971927890831071689768359173260960739254160211017410322799793419223796996260056081828170546988461285168124170297427792046640116184356

main :: IO ()
main = do
  case Dbl.dbl_pow (dec2F x) (dec2F y) of
    TransNumber z -> putStrLn $ "(dbl)  x ^ y = " ++ show (f2Dec z)
    _ -> error "oops"
  case Musl.musl_pow (dec2F x) (dec2F y) of
    TransNumber z -> putStrLn $ "(musl) x ^ y = " ++ show (f2Dec z)
    _ -> error "oops"
  case Dec.dec_pow x y of
    TransNumber z -> putStrLn $ "(dec)  x ^ y = " ++ show z
    _ -> error "oops"

dec2F :: Decimal -> Double
dec2F = fromRational . toRational
f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational
