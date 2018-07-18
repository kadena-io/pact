{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# options_ghc -fno-warn-orphans #-}

module Pact.Analyze.PrenexNormalize (prenexConvert) where

import           Data.Bifunctor     (bimap)
import           Prelude            hiding (Float)

import           Pact.Analyze.Numerical
import           Pact.Analyze.Types
import           Pact.Analyze.Util

#define STANDARD_INSTANCES                                  \
  PureProp Sym{}      -> ([], p);                           \
  PropSpecific Result -> ([], p);                           \
  PVar{}              -> ([], p);                           \
  PAt schema a b ty -> PAt schema a <$> float b <*> pure ty;

instance Float Integer where
  float = floatIntegerQuantifiers

instance Float Bool where
  float = floatBoolQuantifiers

instance Float Decimal where
  float = floatDecimalQuantifiers

instance Float String where
  float = floatStringQuantifiers

instance Float Time where
  float = floatTimeQuantifiers

instance Float Object where
  float p = case p of
    STANDARD_INSTANCES
    PureProp LiteralObject{} -> ([], p)

instance Float KeySet where
  float p = case p of STANDARD_INSTANCES

instance Float Any where
  float p = case p of STANDARD_INSTANCES

flipQuantifier :: Quantifier -> Quantifier
flipQuantifier = \case
  Forall' uid name ty -> Exists' uid name ty
  Exists' uid name ty -> Forall' uid name ty

floatIntegerQuantifiers :: Prop Integer -> ([Quantifier], Prop Integer)
floatIntegerQuantifiers p = case p of
  STANDARD_INSTANCES

  PStrLength pStr -> PStrLength <$> float pStr

  PNumerical (IntArithOp op a b)
    -> PNumerical ... IntArithOp      op <$> float a <*> float b
  PNumerical (IntUnaryArithOp op a)
    -> PNumerical .   IntUnaryArithOp op <$> float a
  PNumerical (ModOp a b)
    -> PNumerical ... ModOp              <$> float a <*> float b
  PNumerical (RoundingLikeOp1 op a)
    -> PNumerical . RoundingLikeOp1 op <$> float a
  PropSpecific (IntCellDelta tn cn a)
    -> PropSpecific . IntCellDelta tn cn <$> float a
  PropSpecific (RowWriteCount tn pRk)
    -> PropSpecific . RowWriteCount tn   <$> float pRk
  PropSpecific (RowReadCount tn pRk)
    -> PropSpecific . RowReadCount tn    <$> float pRk
  PropSpecific (IntColumnDelta{}) -> ([], p)

floatDecimalQuantifiers :: Prop Decimal -> ([Quantifier], Prop Decimal)
floatDecimalQuantifiers p = case p of
  STANDARD_INSTANCES
  PNumerical (DecArithOp op a b)
    -> PNumerical ... DecArithOp      op <$> float a <*> float b
  PNumerical (DecUnaryArithOp op a)
    -> PNumerical .   DecUnaryArithOp op <$> float a
  PNumerical (DecIntArithOp op a b)
    -> PNumerical ... DecIntArithOp   op <$> float a <*> float b
  PNumerical (IntDecArithOp op a b)
    -> PNumerical ... IntDecArithOp   op <$> float a <*> float b
  PNumerical (RoundingLikeOp2 op a b)
    -> PNumerical ... RoundingLikeOp2 op <$> float a <*> float b
  PropSpecific (DecCellDelta tn cn a)
    -> PropSpecific . DecCellDelta tn cn  <$> float a
  PropSpecific (DecColumnDelta{}) -> ([], p)

floatStringQuantifiers :: Prop String -> ([Quantifier], Prop String)
floatStringQuantifiers p = case p of
  STANDARD_INSTANCES
  PStrConcat s1 s2 -> PStrConcat <$> float s1 <*> float s2

floatTimeQuantifiers :: Prop Time -> ([Quantifier], Prop Time)
floatTimeQuantifiers p = case p of
  STANDARD_INSTANCES
  PIntAddTime time int -> PIntAddTime <$> float time <*> float int
  PDecAddTime time dec -> PDecAddTime <$> float time <*> float dec

floatBoolQuantifiers :: Prop Bool -> ([Quantifier], Prop Bool)
floatBoolQuantifiers p = case p of
  STANDARD_INSTANCES

  PropSpecific (Forall uid name ty prop) ->
    let (qs, prop') = float prop
    in (Forall' uid name ty:qs, prop')
  PropSpecific (Exists uid name ty prop) ->
    let (qs, prop') = float prop
    in (Exists' uid name ty:qs, prop')

  PropSpecific Abort              -> ([], p)
  PropSpecific Success            -> ([], p)
  PropSpecific TableWrite{}       -> ([], p)
  PropSpecific TableRead{}        -> ([], p)
  PropSpecific ColumnWrite{}      -> ([], p)
  PropSpecific ColumnRead{}       -> ([], p)
  PropSpecific KsNameAuthorized{} -> ([], p)
  PropSpecific RowEnforced{}      -> ([], p)

  PComparison  op a b -> PComparison  op <$> float a <*> float b
  PObjectEqNeq op a b -> PObjectEqNeq op <$> float a <*> float b
  PKeySetEqNeq op a b -> PKeySetEqNeq op <$> float a <*> float b

  PAnd a b     -> PAnd <$> float a <*> float b
  POr a b      -> POr  <$> float a <*> float b
  PNot a       -> bimap (fmap flipQuantifier) PNot (float a)
  PLogical _ _ -> error ("ill-defined logical op: " ++ show p)

  PropSpecific (RowRead  tn pRk) -> PropSpecific . RowRead  tn <$> float pRk
  PropSpecific (RowWrite tn pRk) -> PropSpecific . RowWrite tn <$> float pRk

reassembleFloated :: [Quantifier] -> Prop Bool -> Prop Bool
reassembleFloated qs prop =
  let mkQuantifiedProp q acc = case q of
        Forall' uid name ty -> PropSpecific (Forall uid name ty acc)
        Exists' uid name ty -> PropSpecific (Exists uid name ty acc)
  in foldr mkQuantifiedProp prop qs

-- We first use @floatBoolQuantifiers@ to remove all quantifiers from the
-- @Prop@ (modifying them as necessary, then put them back in place on the
-- outside of the syntax tree.
--
-- The only interesting cases are those for @Forall@, @Exists@, and @PNot@. In
-- the first two cases, we capture the quantifier to float it up. In the @PNot@
-- case, we flip all the quantifiers found inside the @PNot@ as we lift them
-- over it.
prenexConvert :: Prop Bool -> Prop Bool
prenexConvert = uncurry reassembleFloated . floatBoolQuantifiers
