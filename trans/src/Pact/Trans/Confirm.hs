{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
-- |
-- Module      :  Pact.Trans.Confirmation
-- Copyright   :  (C) 2023 John Wiegley
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  John Wiegley <john@kadena.io>
--
-- 'confirmation' provides a sanity check that the hardware and operating
-- system under which it runs will match the behavior of other nodes.
--

module Pact.Trans.Confirm (confirmation) where

import Data.Decimal
import Data.Maybe
import qualified Pact.Trans.Musl as M

data Case1 = Case1 {
  _arity1_arg :: Decimal,
  _arity1_exp :: Decimal
}

data Case2 = Case2 {
  _arity2_arg1 :: Decimal,
  _arity2_arg2 :: Decimal,
  _arity2_exp  :: Decimal
}

data Case
  = Arity1 String (Double -> Double) [Case1]
  | Arity2 String (Double -> Double -> Double) [Case2]

dec2F :: Decimal -> Double
dec2F = fromRational . toRational
f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

confirmation :: [Case]
confirmation = do
  mapMaybe runCase cases
  where
  runCase :: Case -> Maybe Case
  runCase (Arity1 lbl f cs) =
    fmap (Arity1 lbl f)
      $ wrapList
      $ flip filter cs
      $ \(Case1 x r) ->
            f2Dec (f (dec2F x)) /= r
  runCase (Arity2 lbl f cs) =
    fmap (Arity2 lbl f)
      $ wrapList
      $ flip filter cs
      $ \(Case2 x y r) ->
            f2Dec (f (dec2F x) (dec2F y)) /= r

  wrapList :: [a] -> Maybe [a]
  wrapList [] = Nothing
  wrapList xs = Just xs

f_exp :: [Case1] -> Case
f_exp = Arity1 "exp" M.trans_exp

f_ln :: [Case1] -> Case
f_ln = Arity1 "ln" M.trans_ln

f_logBase :: [Case2] -> Case
f_logBase = Arity2 "logBase" M.trans_log

f_pow :: [Case2] -> Case
f_pow = Arity2 "pow" M.trans_pow

f_sqrt :: [Case1] -> Case
f_sqrt = Arity1 "sqrt" M.trans_sqrt

cases :: [Case]
cases = [
  f_exp [],

  f_ln [],

  f_logBase [],

  f_pow [
    (Case2
      15.034465284692086701747761395233132973944448512421004399685858401206740385711739229018307610943234609057822959334669087436253689423614206061665462283698768757790600552385430913941421707844383369633809803959413869974997415115322843838226312287673293352959835
      3.466120406090666777582519661568003549307295836842780244500133445635634490670936927006970368136648330889718447039413255137656971927890831071689768359173260960739254160211017410322799793419223796996260056081828170546988461285168124170297427792046640116184356
      12020.67042599064370733685791492462158203125)
    ],

  f_sqrt []
  ]
