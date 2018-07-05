{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Pact.Analyze.PrenexNormalize (prenexConvert) where

import           Data.Bifunctor     (bimap)
import           Prelude            hiding (Float)
import           Pact.Types.Lang    hiding (KeySet)

import           Pact.Analyze.Types

data Quantifier
  = Forall' VarId Text QType
  | Exists' VarId Text QType

class Float a where
  float :: Prop a -> ([Quantifier], Prop a)

#define QUANTIFIER_INSTANCES                                           \
  Forall uid name ty prop ->                                           \
    let (qs, prop') = float prop                                       \
    in (Forall' uid name ty:qs, prop');                                \
  Exists uid name ty prop ->                                           \
    let (qs, prop') = float prop                                       \
    in (Exists' uid name ty:qs, prop');

#define STANDARD_INSTANCES                                             \
  PLit{}  -> ([], p);                                                  \
  PSym{}  -> ([], p);                                                  \
  Result  -> ([], p);                                                  \
  PVar _ _ -> ([], p);                                                 \
  PAt schema a b ty -> PAt schema <$> float a <*> float b <*> pure ty;

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
  float p = case p of STANDARD_INSTANCES

instance Float KeySet where
  float p = case p of STANDARD_INSTANCES

flipQuantifier :: Quantifier -> Quantifier
flipQuantifier = \case
  Forall' uid name ty -> Exists' uid name ty
  Exists' uid name ty -> Forall' uid name ty

floatIntegerQuantifiers :: Prop Integer -> ([Quantifier], Prop Integer)
floatIntegerQuantifiers p = case p of
  STANDARD_INSTANCES

  PStrLength pStr -> PStrLength <$> float pStr

  PIntArithOp op a b    -> PIntArithOp      op <$> float a <*> float b
  PIntUnaryArithOp op a -> PIntUnaryArithOp op <$> float a
  PModOp a b            -> PModOp              <$> float a <*> float b
  PRoundingLikeOp1 op a -> PRoundingLikeOp1 op <$> float a
  IntCellDelta tn cn a  -> IntCellDelta tn cn  <$> float a
  RowWriteCount tn pRk  -> RowWriteCount tn    <$> float pRk
  RowReadCount tn pRk   -> RowReadCount tn     <$> float pRk
  IntColumnDelta{}      -> ([], p)

floatDecimalQuantifiers :: Prop Decimal -> ([Quantifier], Prop Decimal)
floatDecimalQuantifiers p = case p of
  STANDARD_INSTANCES
  PDecArithOp op a b      -> PDecArithOp      op <$> float a <*> float b
  PDecUnaryArithOp op a   -> PDecUnaryArithOp op <$> float a
  PDecIntArithOp op a b   -> PDecIntArithOp   op <$> float a <*> float b
  PIntDecArithOp op a b   -> PIntDecArithOp   op <$> float a <*> float b
  PRoundingLikeOp2 op a b -> PRoundingLikeOp2 op <$> float a <*> float b
  DecCellDelta tn cn a    -> DecCellDelta tn cn  <$> float a
  DecColumnDelta{}        -> ([], p)

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
  QUANTIFIER_INSTANCES

  Abort              -> ([], p)
  Success            -> ([], p)
  TableWrite{}       -> ([], p)
  TableRead{}        -> ([], p)
  ColumnWrite{}      -> ([], p)
  ColumnRead{}       -> ([], p)
  KsNameAuthorized{} -> ([], p)
  RowEnforced{}      -> ([], p)

  PIntegerComparison op a b -> PIntegerComparison op <$> float a <*> float b
  PDecimalComparison op a b -> PDecimalComparison op <$> float a <*> float b
  PTimeComparison    op a b -> PTimeComparison    op <$> float a <*> float b
  PBoolComparison    op a b -> PBoolComparison    op <$> float a <*> float b
  PStringComparison  op a b -> PStringComparison  op <$> float a <*> float b
  PKeySetEqNeq       op a b -> PKeySetEqNeq       op <$> float a <*> float b

  PAnd a b     -> PAnd <$> float a <*> float b
  POr a b      -> POr  <$> float a <*> float b
  PNot a       -> bimap (fmap flipQuantifier) PNot (float a)
  PLogical _ _ -> error ("ill-defined logical op: " ++ show p)

  RowRead  tn pRk -> RowRead  tn <$> float pRk
  RowWrite tn pRk -> RowWrite tn <$> float pRk

reassembleFloated :: [Quantifier] -> Prop Bool -> Prop Bool
reassembleFloated qs prop =
  let mkQuantifiedProp q acc = case q of
        Forall' uid name ty -> Forall uid name ty acc
        Exists' uid name ty -> Exists uid name ty acc
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
